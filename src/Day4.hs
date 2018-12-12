{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Day4
    ( mostSleeping
    ) where

import qualified Data.Text as T
import Text.Parsec
    ( many
    , many1
    , manyTill
    , digit
    , eof
    , char
    , parse
    , string
    , anyChar
    , choice
    , try
    , optional
    )
import Text.Parsec.Text (Parser)
import Control.Monad (void)
import Data.List (sort, sortBy)
import Debug.Trace (trace)
import qualified Data.Map.Strict as Map

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)

parser :: Parser [GuardDuty]
parser = do
    result <- many duty
    eof
    return result

timestamp :: Parser Int
timestamp = do
    void $ char '['
    void num
    void $ char '-'
    month <- num
    void $ char '-'
    day <- num
    void $ char ' '
    void num
    void $ char ':'
    minute <- num
    void $ string "] "
    return minute

duty :: Parser GuardDuty
duty = do
    void timestamp
    void $ string "Guard #"
    guardId <- num
    void $ string " begins shift\n"
    sleeps <- many ( try sleep )
    return $ GuardDuty {..}

sleep :: Parser Range
sleep = do
    start <- timestamp
    void $ string "falls asleep\n"
    end <- timestamp
    void $ string "wakes up"
    void $ optional ( char '\n' )
    return (start, end)

type Range = (Int, Int)

data GuardDuty = GuardDuty
    { guardId :: Int
    , sleeps :: [Range]
    }
    deriving Show

type MinuteMap = Map.Map Int Int

minuteList :: GuardDuty -> [Int]
minuteList (GuardDuty { sleeps }) =
    concatMap (\(start, end) -> [start..(end - 1)]) sleeps

markSleeps :: [GuardDuty] -> MinuteMap
markSleeps dutyList = foldl mark Map.empty dutyList where
    updateMap sleepMap point =
        Map.alter update point sleepMap where
            update Nothing = Just 0
            update (Just value) = Just $ value + 1
    mark sleepMap duty = foldl updateMap sleepMap ( minuteList duty )


type GuardMap = Map.Map Int Int

durationById :: [GuardDuty] -> GuardMap
durationById dutyList = foldl updateMap Map.empty dutyList where
    sumSleep sleeps = foldl (+) 0 $ (\(x, y) -> y - x) <$> sleeps
    updateMap guardMap (GuardDuty { .. }) =
        Map.alter update guardId guardMap where
            update Nothing = Just $ sumSleep sleeps
            update (Just value) = Just $ value + (sumSleep sleeps)


getMaxFromMap :: Ord v => Map.Map k v -> Maybe (k,v)
getMaxFromMap m = go Nothing (Map.toList m) where
    go prev [] = prev
    go Nothing ((k,v):rest) = go (Just (k,v)) rest
    go prev@(Just (_,mv)) ((k,v):rest)
        | mv < v    = go (Just (k,v)) rest
        | otherwise = go prev rest

maxOrZero :: Map.Map Int Int -> Int
maxOrZero m = case getMaxFromMap m of
    Just (k, _) -> k
    Nothing -> 0

mostSleeping :: T.Text -> Int
mostSleeping input = bestId * mostSleptMinute where
    sorted = T.intercalate "\n" $ sort ( T.lines input )
    dutyList = case parse parser "day4.txt" sorted of
        Right list -> list
        Left err -> trace ( show err ) []
    bestId = maxOrZero $ durationById dutyList
    sleepMap = markSleeps $ filter (\(GuardDuty { guardId }) -> guardId == bestId ) dutyList
    mostSleptMinute = maxOrZero sleepMap
