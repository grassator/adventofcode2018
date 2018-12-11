module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)


run :: (Show a, Show b) => String -> (T.Text -> a) -> (T.Text -> b) -> IO ()
run day first second = do
    text <- TIO.readFile $ "./data/day" ++ day ++ ".txt"
    putStrLn $ "1: " ++ ( show ( first text ) )
    putStrLn $ "2: " ++ ( show ( second text ) )

main :: IO ()
main = do
    args <- getArgs
    case args of
        "1":_ -> run "1" Day1.calibrate Day1.findRepeating
        "2":_ -> run "2" Day2.checksum Day2.offByOne
        "3":_ -> run "3" Day3.countOverlapping Day3.findNonOverlapping
        _ -> do
            putStrLn "Please select a day, e.g:"
            putStrLn "  adventofcode 1"
