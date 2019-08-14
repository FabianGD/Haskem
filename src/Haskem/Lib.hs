{-# LANGUAGE OverloadedStrings #-}

module Haskem.Lib
    ( gaussianParser,
      isEmpty,
      trim,
      doParsing,
    ) where

import              IRC2NMode.Types
import              Control.Applicative
import              Data.Attoparsec.Text.Lazy   hiding (take)
import qualified    Data.Text                   as T
import qualified    Data.Text.IO                as TIO
import              Data.Char
import              Data.List
import              Lens.Micro.Platform


gaussianParser :: Parser GaussianInfo
gaussianParser = do
    -- TODO --> Read XYZ section in Gaussian files

    -- Find the SCF Done tag
    _ <- manyTill anyChar (string "SCF Done:") *> skipSpace

    -- Read the method tag from the following brackets
    method' <- string "E(" *> manyTill anyChar (string ")")
    _ <- skipSpace *> string "=" *> skipSpace
    
    -- Read the SCF energy
    scfEnergy' <- double
    -- _ <- manyTill anyChar endOfLine
    
    -- Return the data to the GaussianInfo data type
    return GaussianInfo {
        _method     = method',
        _scfEnergy  = scfEnergy'
    }


isEmpty :: String -> Bool
isEmpty = all isSpace


trim :: String -> String
trim input = reverse flippedTrimmed
    where
        trimStart = dropWhile isSpace input         -- Drop leading whitespaces
        flipped = reverse trimStart                 -- Rev. lead-trimmed str
        flippedTrimmed = dropWhile isSpace flipped  -- Drop trailing whitespaces


doParsing :: FilePath -> IO GaussianInfo
doParsing filePath = do
    fileContent <- TIO.readFile filePath
    let gaussParse = parseOnly gaussianParser fileContent

    case gaussParse of 
        Left    err     -> error "Nope"
        Right   gInfo   -> return gInfo
    