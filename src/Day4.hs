{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Day4
    ( mostSleeping
    , mostFrequentMinute
    ) where

import qualified Data.Text as T
import Text.Parsec
    ( many
    , many1
    , digit
    , eof
    , char
    , parse
    , string
    , try
    , optional
    )
import Text.Parsec.Text (Parser)
import Control.Monad (void)
import Data.List (sort)
import Debug.Trace (trace)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

num :: Parser Int
num = read <$> many1 digit

parser :: Parser [GuardDuty]
parser = many duty <* eof

timestamp :: Parser Int
timestamp = do
    void $ char '[' >> num >> char '-' >> num >> char '-' >> num
        >> char ' ' >> num >> char ':'
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

minuteListFromRanges :: [Range] -> [Int]
minuteListFromRanges = concatMap (\(start, end) -> [start..(end - 1)])

aggregateBy :: (Ord k) => (a -> k) -> (Maybe b -> a -> b) -> [a] -> Map.Map k b
aggregateBy getKey update list = foldl go Map.empty list where
    go aMap x = Map.alter (\value -> Just $ update value x) (getKey x) aMap

markSleeps :: [GuardDuty] -> MinuteMap
markSleeps = go . concatMap ( minuteListFromRanges . sleeps ) where
    go = aggregateBy id $ const . \case
        Nothing -> 0
        Just value -> value + 1

type GuardMap = Map.Map Int Int
type RangeMap = Map.Map Int [Range]

rangesById :: [GuardDuty] -> RangeMap
rangesById = aggregateBy guardId $ \case
        Nothing -> sleeps
        Just value -> (++) value . sleeps

durationById :: [GuardDuty] -> GuardMap
durationById m = Map.map sumRanges $ rangesById m where
    sumRanges = foldl (+) 0 . fmap (\(x, y) -> y - x)

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


sleepMapFromRanges :: [Range] -> MinuteMap
sleepMapFromRanges sleeps = foldl updateMap Map.empty ( minuteListFromRanges sleeps ) where
    updateMap sleepMap minute =
        Map.alter update minute sleepMap where
            update Nothing = Just 0
            update (Just value) = Just $ value + 1

mostFrequentMinute :: T.Text -> Int
mostFrequentMinute input = theId * theMinute where
    sorted = T.intercalate "\n" $ sort ( T.lines input )
    dutyList = case parse parser "day4.txt" sorted of
        Right list -> list
        Left err -> trace ( show err ) []
    rangesMap = rangesById dutyList
    minuteMapMap = Map.map sleepMapFromRanges rangesMap where
    minuteMap = Map.map rangesToMinutes minuteMapMap where
        rangesToMinutes m = case getMaxFromMap m of
            Just (_, v) -> v
            Nothing -> 0
    theId = maxOrZero minuteMap
    theMinute = maxOrZero ( fromMaybe Map.empty ( Map.lookup theId minuteMapMap ) )
