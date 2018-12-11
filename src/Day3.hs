{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day3
    ( countOverlapping
    , findNonOverlapping
    ) where

import qualified Data.Text as T
import Text.Parsec (many, many1, digit, eof, char, parse, string)
import Text.Parsec.Text (Parser)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)
import Data.List (find)

data Area = Area
    { areaId :: Int
    , areaX :: Int
    , areaY :: Int
    , areaWidth :: Int
    , areaHeight :: Int
    }
    deriving Show

num :: Parser Int
num = do
    n <- many1 digit
    return (read n)


parser :: Parser [Area]
parser = do
    result <- many line
    eof
    return result

line :: Parser Area
line = do
    void $ char '#'
    areaId <- num
    void $ string " @ "
    areaX <- num
    void $ char ','
    areaY <- num
    void $ string ": "
    areaWidth <- num
    void $ char 'x'
    areaHeight <- num
    void $ char '\n'
    return $ Area {..}

type ClothMap = Map.Map (Int, Int) Int

coords :: Area -> [(Int, Int)]
coords (Area {..}) = concatMap
    (\x -> fmap (x,) [areaY..(areaY + areaHeight - 1)])
    [areaX..(areaX + areaWidth - 1)]

markOverlaps :: [Area] -> ClothMap
markOverlaps areaList = foldl mark Map.empty areaList where
    updateMap markMap point =
        Map.alter update point markMap where
            update Nothing = Just 0
            update _ = Just 1
    mark markMap area = foldl updateMap markMap ( coords area )

parseAndProcessAreas :: ([Area] -> Int) -> T.Text -> Int
parseAndProcessAreas process input =
    case parse parser "" input of
        Right areaList -> process areaList
        Left err -> trace (show err) 0

countOverlapping :: T.Text -> Int
countOverlapping = parseAndProcessAreas $ Map.foldr' (+) 0 . markOverlaps

findNonOverlapping :: T.Text -> Int
findNonOverlapping input = parseAndProcessAreas go input where
    go areaList = fromMaybe 0 $ fmap areaId found where
        markMap = markOverlaps areaList
        found :: Maybe Area
        found = find ( nonOverapping . coords ) areaList
        nonOverapping points = all areZero points where
            areZero point = ( fromMaybe 0 $ Map.lookup point markMap ) == 0
