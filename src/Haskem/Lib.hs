{-# LANGUAGE OverloadedStrings #-}

module Haskem.Lib
    ( trim,
      doGaussianParsing,
      doFreqParsing,
      doGenericParsing
    ) where

import              Haskem.Types
import              Control.Applicative
import              Data.Attoparsec.Text.Lazy   hiding (take)
import qualified    Data.Text                   as T 
import qualified    Data.Text.IO                as TIO
import              Data.Char
import              Data.List                   hiding (takeWhile)
import              Lens.Micro.Platform
import              Prelude                     hiding (takeWhile)
import              Debug.Trace


convertElemSym2Num :: String -> Int
convertElemSym2Num "H"  = 1
convertElemSym2Num "He" = 2
convertElemSym2Num "Li" = 3
convertElemSym2Num "Be" = 4
convertElemSym2Num "B"  = 5
convertElemSym2Num "C"  = 6
convertElemSym2Num "N"  = 7
convertElemSym2Num "O"  = 8
convertElemSym2Num "S"  = 16
convertElemSym2Num _    = 0




parseXYZFile :: Parser Molecule
parseXYZFile = do
    skipSpace
    nrOfAtoms' <- decimal

    _ <- takeWhile (not <$> isEndOfLine) *> endOfLine
    _ <- takeWhile (not <$> isEndOfLine) *> endOfLine

    atoms' <- count nrOfAtoms' $ do
        skipSpace
        
        elementSym' <- manyTill anyChar space
        let elemNum' = convertElemSym2Num elementSym'

        skipSpace
        coordx' <- double <* skipSpace
        coordy' <- double <* skipSpace
        coordz' <- double
        
        _ <- takeWhile (not <$> isEndOfLine) *> endOfLine

        return Atom {
            _centerNumber   = Nothing,
            _atomNumber     = elemNum',
            _coordinate     = (coordx', coordy', coordz')
        }

    return Molecule {
        _molName    = Nothing,
        _nrOfAtoms  = nrOfAtoms',
        _atoms      = atoms'
    }

gaussianParseXYZ :: Parser Atom
gaussianParseXYZ = do
    -- Skip inital whitespace
    skipSpace

    -- Center number is not required
    centerNumber' <- decimal <* skipSpace
    
    -- Atom (element) number
    atomNumber' <- decimal <* skipSpace

    -- Atomic Type is irrelevant
    _ <- decimal <* skipSpace

    -- Coordinates
    coordx' <- double <* skipSpace
    coordy' <- double <* skipSpace
    coordz' <- double <* skipSpace

    _ <- takeWhile (not <$> isEndOfLine) *> endOfLine

    return Atom {
            _centerNumber   = Just centerNumber',
            _atomNumber     = atomNumber',
            _coordinate     = (coordx', coordy', coordz')
        }

gaussianParser :: Parser GaussianInfo
gaussianParser = do
    -- TODO --> Read Internal coordinates
    
    -- ==> Read XYZ section in Gaussian files
    _ <- manyTill anyChar (string "Standard orientation") 
    
    _ <- count 5 $ do 
        _ <- takeWhile (not <$> isEndOfLine)
        endOfLine    

    -- Arriving at first digit
    -- skipSpace

    -- Read all atoms
    atoms' <- many1 $ gaussianParseXYZ

    nrOfAtoms' <- parseNAtoms

    let molecule' = Molecule {
        _molName = Nothing,
        _nrOfAtoms = nrOfAtoms',
        _atoms = atoms'    
    }

    -- Find the SCF Done tag
    _ <- manyTill anyChar (string "SCF Done:") <* skipSpace

    -- ==> Read the method tag from the following brackets
    method' <- string "E(" *> manyTill anyChar (string ")")
    _ <- skipSpace *> string "=" *> skipSpace
    
    -- Read the SCF energy
    scfEnergy' <- double
    -- _ <- manyTill anyChar endOfLine
    
    -- Return the data to the GaussianInfo data type
    return GaussianInfo {
        _method     = method',
        _scfEnergy  = scfEnergy',
        _molecule   = Just molecule'
    }


parseNAtoms :: Parser Int
parseNAtoms = manyTill anyChar (string "NAtoms=") *> skipSpace *> decimal 


gaussianFreqParser :: Parser [NormalMode]
gaussianFreqParser = do 

    nrAtoms' <- parseNAtoms
    _ <- manyTill anyChar (string "normal coordinates:")

    -- traceShowM test'

    _ <- takeWhile (not <$> isEndOfLine)
    endOfLine    

    normalModes' <- count (nrAtoms' - 2) $ do
        modeNumbers' <- count 3 $ do
            skipSpace
            decimal

        traceShowM modeNumbers'

        _ <- takeWhile (not <$> isEndOfLine)
        endOfLine     

        symmetries' <- count 3 $ do
            skipSpace                   -- Whitespaces before
            manyTill anyChar space      -- Symmetry symbol

        -- Skip until the next block
        _ <- manyTill anyChar (string "--")     
        
        traceShowM symmetries'

        frequencies' <- count 3 $ do
            skipSpace
            double                      -- Take the frequency

        traceShowM frequencies'

        _ <- manyTill anyChar (string "--")     
    
        reducedMass' <- count 3 $ do
            skipSpace
            double                      -- Take the reduced mass
    
        traceShowM reducedMass'

        _ <- manyTill anyChar (string "--")     

        forceConst' <- count 3 $ do
            skipSpace
            double                      -- Take the force constant
            
        traceShowM forceConst'

        _ <- manyTill anyChar (string "--")     

        irIntensity' <- count 3 $ do
            skipSpace
            double                      -- Take the IR intensity
            
        traceShowM irIntensity'

        _ <- takeWhile (not <$> isEndOfLine)
        endOfLine     

        -- Skip header
        _ <- takeWhile (not <$> isEndOfLine)
        endOfLine 

        atoms' <- count nrAtoms' $ do
            -- Parse atom displacements line by line
            skipSpace
            centerNum' <- decimal <* skipSpace
            atomNum' <- decimal
            
            -- traceShowM [centerNum', atomNum']

            count 3 $ do
                skipSpace
                xcoord' <- double <* skipSpace
                ycoord' <- double <* skipSpace
                zcoord' <- double

                return Atom {
                    _centerNumber   = Just centerNum',
                    _atomNumber     = atomNum',
                    _coordinate     = (xcoord', ycoord', zcoord')
                }

        let atomsOrdered' = transpose atoms'
        
        traceShowM $ length <$> atomsOrdered'

        return [
            NormalMode {
                _frequency      = frequencies' !! ii,
                _redMass        = Just $ reducedMass' !! ii,
                _forceConst     = Just $ forceConst' !! ii,
                _irIntensity    = Just $ irIntensity' !! ii,
                _displacement   = Just $ atomsOrdered' !! ii
            } | ii <- [0..2]]
       
    traceShowM $ length <$> normalModes'
    
    -- return normalModes'
    return $ concat normalModes'

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


doGenericParsing :: FilePath -> Parser a -> IO a
doGenericParsing filePath parser = do
    fileContent <- TIO.readFile filePath
    let gParse = parseOnly parser fileContent

    case gParse of 
        Left    err     -> error err
        Right   gInfo   -> return gInfo
