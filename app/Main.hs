module Main where

import qualified Day1
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
