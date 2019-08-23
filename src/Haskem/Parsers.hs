{-# LANGUAGE OverloadedStrings #-}

module Haskem.Parsers(
    xyzParser,
    gaussianFreqParser,
    gaussianParser,
    elemNum2Sym,
    elemSym2Num,
    elemMass,
    vdWRadius,
    covRadius
) where

import              Haskem.Types
import              Data.Attoparsec.Text.Lazy   hiding (take)
import              Prelude                     hiding (takeWhile)
import qualified    Data.Map.Strict             as Map
import              Data.Maybe
import              Data.Tuple.HT               hiding (double, triple)
import              Debug.Trace
import              Data.List                   hiding (takeWhile)


elemSym2Num :: String -> Int
elemSym2Num elemSym = fromMaybe 0 $ Map.lookup elemSym elemMap  
    where elemMap = Map.fromList [("H", 1),     ("He", 2),  ("Li", 3), 
                                  ("Be", 4),    ("B", 5),   ("C", 6),
                                  ("N", 7),     ("O", 8),   ("F", 9), 
                                  ("Ne", 10),   ("Na", 11), ("Mg", 12),
                                  ("Al", 13),   ("Si", 14), ("P", 15),
                                  ("S", 16),    ("Cl", 17), ("Ar", 18)]

elemNum2Sym :: Int -> String
elemNum2Sym elemSym = fromMaybe "" $ Map.lookup elemSym elemMap  
    where elemMap = Map.fromList $ map swap [("H", 1),     ("He", 2),  ("Li", 3), 
                                             ("Be", 4),    ("B", 5),   ("C", 6),
                                             ("N", 7),     ("O", 8),   ("F", 9), 
                                             ("Ne", 10),   ("Na", 11), ("Mg", 12),
                                             ("Al", 13),   ("Si", 14), ("P", 15),
                                             ("S", 16),    ("Cl", 17), ("Ar", 18)]


elemMass :: Int -> Double
elemMass elemNum = fromMaybe 1 $ Map.lookup elemNum elemMap  
    where elemMap = Map.fromList [(1, 1.0079),      (2, 4.0026),    (3, 6.941), 
                                  (4, 9.0122),      (5, 10.811),    (6, 12.0107),
                                  (7, 14.0067),     (8, 15.9994),   (9, 18.9984),
                                  (10, 20.1797),    (11, 22.9897),  (12, 24.305),
                                  (13, 26.9815),    (14, 28.0855),  (15, 30.9738),
                                  (16, 32.065),     (17, 35.453),   (18, 39.948)]


vdWRadius :: Int -> Double
vdWRadius elemNum = fromMaybe 1.2 $ Map.lookup elemNum vdWMap
    where vdWMap = Map.fromList [(1, 1.20),     (2, 1.40),  (3, 1.82),  (4, 1.53),
                                 (5, 1.92),     (6, 1.70),  (7, 1.55),  (8, 1.52),  
                                 (9, 1.47),     (10, 1.54), (11, 2.27), (12, 1.73),   
                                 (13, 1.84),    (14, 2.10), (15, 1.80), (16, 1.80),
                                 (17, 1.75),    (18, 1.88)]


covRadius :: Int -> Double
covRadius elemNum = fromMaybe 1.2 $ Map.lookup elemNum vdWMap
    where vdWMap = Map.fromList [(1, 0.31),     (2, 0.28),  (3, 0.96),  (4, 0.96),
                                (5, 0.84),     (6, 0.73),  (7, 0.71),  (8, 0.66),  
                                (9, 0.57),     (10, 0.58), (11, 1.66), (12, 1.41),   
                                (13, 1.21),    (14, 1.11), (15, 1.07), (16, 1.05),
                                (17, 1.02),    (18, 1.06)]


xyzParser :: Parser Molecule
xyzParser = do
    skipSpace
    nrOfAtoms' <- decimal

    _ <- takeWhile (not <$> isEndOfLine) *> endOfLine
    _ <- takeWhile (not <$> isEndOfLine) *> endOfLine

    atoms' <- count nrOfAtoms' $ do
        skipSpace

        elementSym' <- manyTill anyChar space
        let elemNum' = elemSym2Num elementSym'

        skipSpace
        coordx' <- double <* skipSpace
        coordy' <- double <* skipSpace
        coordz' <- double

        _ <- takeWhile (not <$> isEndOfLine) *> endOfLine

        return Atom {
            _centerNumber   = Nothing,
            _atomNumber     = elemNum',
            _coordinate     = CartesianCoord coordx' coordy' coordz'
        }

    return Molecule {
        _molName    = "",
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
            _coordinate     = CartesianCoord coordx' coordy' coordz'
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
        _molName = "",
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
                    _coordinate     = CartesianCoord xcoord' ycoord' zcoord'
                }

        let atomsOrdered' = transpose atoms'

        traceShowM $ length <$> atomsOrdered'

        return [
            NormalMode {
                _frequency      = frequencies' !! ii,
                _idx            = Just $ modeNumbers' !! ii,
                _redMass        = Just $ reducedMass' !! ii,
                _forceConst     = Just $ forceConst' !! ii,
                _irIntensity    = Just $ irIntensity' !! ii,
                _displacement   = atomsOrdered' !! ii
            } | ii <- [0..2]]

    traceShowM $ length <$> normalModes'

    -- return normalModes'
    return $ concat normalModes'
