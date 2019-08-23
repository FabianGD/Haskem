{-# LANGUAGE OverloadedStrings #-}

module Haskem.Lib
    ( doGaussianParsing,
      doFreqParsing,
      doGenericParsing,
      xyzParser,
      gaussianFreqParser,
      gaussianParser,
      elemMass,
      elemSym2Num,
      calculateCoM,
      calcDisplacementVector,
      writeMoleculeToXYZ,
      projectDonN,
      calcProjectionVec,
      calcLength3D,
      setUpBonds,
      setUpBonds',
      setUpBonds'',
      setUpBonds''',
      checkBondCompleteness
    ) where

import              Haskem.Parsers
import              Haskem.Types
import              Control.Applicative
import qualified    Formatting                  as F
import              Data.Attoparsec.Text.Lazy   hiding (take)
import qualified    Data.Text.Lazy              as T
import qualified    Data.Text.IO                as TIO
import              Data.Char
import              Lens.Micro.Platform
import              Debug.Trace
import              Data.List                   hiding (takeWhile)
import              Data.List.Extra            
-- import              Prelude                     hiding (takeWhile)
-- import              Data.Tuple.HT               hiding (double, triple)
import qualified    Data.Map.Strict             as Map
import qualified    Data.Vector.Storable        as VS
import qualified    Numeric.LinearAlgebra.Data  as LA
import qualified    Data.IntMap                 as IM
import qualified    Data.IntSet                 as IS

xyzFormat :: F.Format r (String -> Double -> Double -> Double -> r)
xyzFormat = F.string F.% "\t" F.% F.fixed 4 F.% "\t" F.% F.fixed 4 F.% "\t" F.% F.fixed 4


doGaussianParsing :: FilePath -> IO GaussianInfo
doGaussianParsing filePath = do
    fileContent <- TIO.readFile filePath
    let gaussParse = parseOnly gaussianParser fileContent

    case gaussParse of
        Left    err     -> error err
        Right   gInfo   -> return gInfo


-- Parses a Gaussian freq calculation output to a GaussianInfo instance and a 
-- list of Normalmode instances. Returns the output as a tuple in IO.
doFreqParsing :: FilePath -> IO (GaussianInfo, [NormalMode])
doFreqParsing filePath = do
    fileContent <- TIO.readFile filePath

    let gaussParse = parseOnly gaussianParser       fileContent
    let gaussFreq  = parseOnly gaussianFreqParser   fileContent

    gFreq <- case gaussFreq of
        Left    err     -> error err
        Right   gFreq   -> return gFreq

    gInfo <- case gaussParse of
        Left    err     -> error err
        Right   gInfo   -> return gInfo

    return (gInfo, gFreq)


-- Parses a file using a specified parser and returns the result in IO
doGenericParsing :: FilePath -> Parser a -> IO a
doGenericParsing filePath parser = do
    fileContent <- TIO.readFile filePath
    let gParse = parseOnly parser fileContent

    case gParse of
        Left    err     -> error err
        Right   gInfo   -> return gInfo


getListOfCoords :: Molecule -> [CartesianCoord]
getListOfCoords molA' = molA' ^.. atoms . each . coordinate


-- Calculates the center of mass of a given molecule of type Molecule (from Haskem.Types)molA
calculateCoM :: Molecule -> CartesianCoord
calculateCoM molecule' = CartesianCoord xCoM yCoM zCoM
    where 
        atomsList   = getListOfCoords molecule' 
        massList    = map elemMass $ molecule' ^.. atoms . each . atomNumber
        fullMass    = sum massList
        xCoM        = (/ fullMass) $ sum $ zipWith (*) massList $ map (^. xCoord) atomsList
        yCoM        = (/ fullMass) $ sum $ zipWith (*) massList $ map (^. yCoord) atomsList
        zCoM        = (/ fullMass) $ sum $ zipWith (*) massList $ map (^. zCoord) atomsList


-- Calculate a displacement vector from two molecular geometries. 
-- Outputs a maybe molecule (in case the geometries do not match 
-- (checked via number of atoms and the list of element numbers))
calcDisplacementVector :: Molecule -> Molecule -> [CartesianCoord]
calcDisplacementVector molA molB = 
    do
        let nrOfAtomsA  = molA ^. nrOfAtoms
            nrOfAtomsB  = molA ^. nrOfAtoms
            elemA   = molA ^.. atoms . each . atomNumber 
            elemB   = molB ^.. atoms . each . atomNumber 
        if (nrOfAtomsA == nrOfAtomsB) && (elemA == elemB)
            then do 
                let atomsA  = getListOfCoords molA
                    atomsB  = getListOfCoords molB
                    xD      = zipWith (-) (map (^. xCoord) atomsA) $ map (^. xCoord) atomsB
                    yD      = zipWith (-) (map (^. yCoord) atomsA) $ map (^. yCoord) atomsB
                    zD      = zipWith (-) (map (^. zCoord) atomsA) $ map (^. zCoord) atomsB
                zipWith3 CartesianCoord xD yD zD
            else 
                []


-- Writes a "Molecule" back to disk as a .xyz file                 
writeMoleculeToXYZ :: Molecule -> FilePath -> IO ()
writeMoleculeToXYZ molA fp = writeFile fp (show nrOfAtomsA 
                                ++ "\nThanks for using Haskem!\n" 
                                ++ T.unpack (T.unlines lineList))
    where
        coordinates = getListOfCoords molA
        nrOfAtomsA  = molA ^. nrOfAtoms
        elems       = map elemNum2Sym $ molA ^.. atoms . each . atomNumber
        lineList    = zipWith4 (F.format xyzFormat)
                        elems 
                        (map (^. xCoord) coordinates) 
                        (map (^. yCoord) coordinates) 
                        (map (^. zCoord) coordinates)
                                

projectDonN :: [CartesianCoord] -> NormalMode -> Double
projectDonN displCoordsA modesA = projectCoordNM
    where
        displCoordsNM   = modesA ^.. displacement . each .coordinate  :: [CartesianCoord]
        projectCoordNM  = sum [sum $ zipWith (*) 
                            (map ( ^. cc) displCoordsA) 
                            (map ( ^. cc) displCoordsNM) | cc <- [xCoord, yCoord, zCoord]] 


calcProjectionVec :: [NormalMode] -> [CartesianCoord] -> [Double]
calcProjectionVec nModes displCoords = projectionVec
        where
            projectionVec = map (projectDonN displCoords) nModes


{-
Build redundant internal coordinates from the molecular cartesian structure
-}

calcLength3D :: CartesianCoord -> CartesianCoord -> Double
calcLength3D coord1 coord2 = length3D
        where
            length3D = sqrt . sum $ (** 2) <$> dVector 
            dVector = [(coord1 ^. axis) - (coord2 ^. axis) 
                            | axis <- [xCoord, yCoord, zCoord]]
               

setUpBonds :: Molecule -> [(String, String, Double)]
setUpBonds molA = bondsList
        where
            atomListN   = molA ^.. atoms . each . atomNumber
            atomListS   = map elemNum2Sym   atomListN
            vdWList     = map covRadius     atomListN
            enumCoords  =  zip4 [1..] atomListS vdWList $ getListOfCoords molA
            bondsList   = [(e1 ++ show i1, e2 ++ show i2, calcLength3D c1 c2) 
                            | (i1, e1, r1, c1) <- enumCoords,
                              (i2, e2, r2, c2) <- enumCoords,
                              let l12 = calcLength3D c1 c2,
                              i1 < i2 && l12 < 1.3 * (r1 + r2)
                            ]  


setUpBonds' :: Molecule -> [LinearBond]
setUpBonds' molA = bondsList
        where
            atomListN'   = molA ^.. atoms . each . atomNumber
            atomListS   = map elemNum2Sym   atomListN'
            vdWList     = map covRadius     atomListN'
            enumCoords  =  zip4 [1..] atomListS vdWList $ getListOfCoords molA
            bondsList   = [LinearBond (AtomSpec e1 i1) (AtomSpec e2 i2) l12
                            | (i1, e1, r1, c1) <- enumCoords,
                              (i2, e2, r2, c2) <- enumCoords,
                              i1 < i2,
                              let l12 = calcLength3D c1 c2,
                              l12 <= 1.3 * (r1+r2)] 


setUpBonds'' :: Molecule -> LA.Matrix Double
setUpBonds'' molA = LA.reshape nrOfAtoms' bondsMatrix
        where
            nrOfAtoms'  = molA ^. nrOfAtoms
            atomListN'  = molA ^.. atoms . each . atomNumber
            atomListS   = map elemNum2Sym   atomListN'
            vdWList     = map covRadius     atomListN'
            enumCoords  = zip4 [1..] atomListS vdWList $ getListOfCoords molA
            bondsMatrix = VS.fromList [bl | (i1, e1, r1, c1) <- enumCoords,
                                            (i2, e2, r2, c2) <- enumCoords,
                                            let l12 = calcLength3D c1 c2,
                                            let bl  = if l12 < 1.3 * (r1+r2) then 1 else 0]


setUpBonds''' :: Molecule -> [IS.IntSet]
setUpBonds''' molA = bondsMatrix
        where
            nrOfAtoms'  = molA ^. nrOfAtoms
            atomListN'  = molA ^.. atoms . each . atomNumber
            atomListS   = map elemNum2Sym   atomListN'
            vdWList     = map covRadius     atomListN'
            enumCoords  = zip4 [1..] atomListS vdWList $ getListOfCoords molA
            bondsMatrix = IS.fromList <$> chunksOf nrOfAtoms' [bl | 
                                                (i1, e1, r1, c1) <- enumCoords,
                                                (i2, e2, r2, c2) <- enumCoords,
                                                let l12 = calcLength3D c1 c2,
                                                let bl  = if l12 < 1.3 * (r1+r2) then 1 else 0]
-- TODO!


removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort


checkBondCompleteness :: Molecule -> [LinearBond] -> [Atom]
checkBondCompleteness mol' listOfBonds' = missing    
        where
            nrOfAtoms'      = mol' ^. nrOfAtoms
            atomNums'       = removeDuplicates $ map (^. bAtomA . geomNum) listOfBonds'  
                                ++ map (^. bAtomB . geomNum) listOfBonds'
            missingIndxs    = findIndices (`notElem` atomNums')  [1..nrOfAtoms']
            missing         = case missingIndxs of  
                               []   -> []
                               _    -> concat [mol' ^.. atoms . ix ii | ii <- missingIndxs]


defineFragments     :: [IS.IntSet] -> [IS.IntSet]
defineFragments []      = []
defineFragments [x]     = [x]

defineFragments (a:as)  = 
    if (all null overlaps)
        then (a:as)
        else defineFragments reducedSet
    where
        setsWithOverlap ::  IS.IntSet -> [IS.IntSet] -> [IS.IntSet]
        setsWithOverlap bond bondsList = [ii | ii <- bondsList,
                                                not . IS.null $ IS.intersection bond ii,
                                                bond /= ii]

        overlaps = 
            [setsWithOverlap ((a:as) !! ii) (deleteNth ii (a:as))
                | ii <- [0..length(a:as) - 1]
            ] 
            :: [[IS.IntSet]]

        reducedSet =
            nub
            [ IS.unions (((a:as) !! i) : (overlaps !! i))
                | i <- [0 .. length (a:as) - 1]
            ]
        
            
deleteNth :: Int -> [a] -> [a]
deleteNth n l = (take n l) ++ (drop (n + 1) l)

-- defineFragments'    :: VS.Vector (IM.IntMap Int) -> VS.Vector (IM.IntMap Int)
-- defineFragments'