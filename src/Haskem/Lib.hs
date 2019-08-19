{-# LANGUAGE OverloadedStrings #-}

module Haskem.Lib
    ( trim,
      doGaussianParsing,
      doFreqParsing,
      doGenericParsing,
      xyzParser,
      gaussianFreqParser,
      gaussianParser,
      elemMass,
      elemSym2Num,
      calculateCoM,
      calcDisplacementVector,
      writeMoleculeToXYZ
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
-- import              Prelude                     hiding (takeWhile)
-- import              Data.Tuple.HT               hiding (double, triple)
-- import qualified    Data.Map.Strict             as Map


xyzFormat :: F.Format r (String -> Double -> Double -> Double -> r)
xyzFormat = F.string F.% "\t" F.% F.fixed 4 F.% "\t" F.% F.fixed 4 F.% "\t" F.% F.fixed 4


trim :: String -> String
trim input = reverse flippedTrimmed
    where
        trimStart = dropWhile isSpace input         -- Drop leading whitespaces
        flipped = reverse trimStart                 -- Rev. lead-trimmed str
        flippedTrimmed = dropWhile isSpace flipped  -- Drop trailing whitespaces


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


calculateCoM :: Molecule -> CartesianCoord
-- Calculates the center of mass of a given molecule of type Molecule (from Haskem.Types)
calculateCoM molecule' = CartesianCoord xCoM yCoM zCoM
    where 
        atomsList   = getListOfCoords molecule' 
        massList    = map elemMass $ molecule' ^.. atoms . each . atomNumber
        fullMass    = sum massList
        xCoM        = (/ fullMass) $ sum $ zipWith (*) massList $ map (^. xCoord) atomsList
        yCoM        = (/ fullMass) $ sum $ zipWith (*) massList $ map (^. yCoord) atomsList
        zCoM        = (/ fullMass) $ sum $ zipWith (*) massList $ map (^. zCoord) atomsList


calcDisplacementVector :: Molecule -> Molecule -> Maybe Molecule
calcDisplacementVector molA molB = 
    do
        let nrOfAtomsA  = molA ^. nrOfAtoms
        let nrOfAtomsB  = molA ^. nrOfAtoms
        let elemA   = molA ^.. atoms . each . atomNumber 
        let elemB   = molB ^.. atoms . each . atomNumber 
        if (nrOfAtomsA == nrOfAtomsB) && (elemA == elemB)
            then do 
                let atomsA  = getListOfCoords molA
                let atomsB  = getListOfCoords molB
                let xD      = zipWith (-) (map (^. xCoord) atomsA) $ map (^. xCoord) atomsB
                let yD      = zipWith (-) (map (^. yCoord) atomsA) $ map (^. yCoord) atomsB
                let zD      = zipWith (-) (map (^. zCoord) atomsA) $ map (^. zCoord) atomsB
                let zipXYZ  = zipWith3 CartesianCoord xD yD zD
                let atomsD  = zipWith3 Atom [Nothing | _ <- elemA] elemA zipXYZ
                Just $ Molecule "" nrOfAtomsA atomsD
            else 
                Nothing


writeMoleculeToXYZ :: Molecule -> FilePath -> IO ()
writeMoleculeToXYZ molA fp = 
    do
        let coordinates     = getListOfCoords molA
        let nrOfAtomsA      = molA ^. nrOfAtoms
        let elems           = map elemNum2Sym $ molA ^.. atoms . each . atomNumber
        let lineList        = zipWith4 (F.format xyzFormat)
                                elems 
                                (map (^. xCoord) coordinates) 
                                (map (^. yCoord) coordinates) 
                                (map (^. zCoord) coordinates)
                                
        writeFile fp (show nrOfAtomsA 
                        ++ "\nThanks for using Haskem!\n" 
                        ++ T.unpack (T.unlines lineList))