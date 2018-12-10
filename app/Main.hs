module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            putStrLn "Please select a day, e.g:"
            putStrLn "  adventofcode 1"
        "1":_ -> do
            text <- TIO.readFile "./data/day1.txt"
            putStrLn $ "1: " ++ ( show ( Day1.calibrate text ) )
            putStrLn $ "2: " ++ ( show ( Day1.findRepeating text ) )
        "2":_ -> do
            text <- TIO.readFile "./data/day2.txt"
            putStrLn $ "1: " ++ ( show ( Day2.checksum text ) )
            putStrLn $ "2: " ++ ( show ( Day2.offByOne text ) )
        "3":_ -> do
            text <- TIO.readFile "./data/day3.txt"
            putStrLn $ "1: " ++ ( show ( Day3.findOverlapping text ) )
