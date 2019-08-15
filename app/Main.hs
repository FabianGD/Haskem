module Main where

import Haskem.Lib

main :: IO ()
main = do 
    parsedData <- doFreqParsing "/scratch/tmp/gaussian/FC-B35/freq/T123_TD_26_16_FC-B35.log"

    return ()