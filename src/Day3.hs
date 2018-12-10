{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day3
    ( findOverlapping
    ) where

import qualified Data.Text as T
import Text.Parsec (many, many1, digit, eof, char, parse, string)
import Text.Parsec.Text (Parser)
import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

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
    identifier <- num
    void $ string " @ "
    x <- num
    void $ char ','
    y <- num
    void $ string ": "
    width <- num
    void $ char 'x'
    height <- num
    void $ char '\n'
    return $ Area
        { areaId = identifier
        , areaX = x
        , areaY = y
        , areaWidth = width
        , areaHeight = height
        }

type ClothMap = Map.Map (Int, Int) Int

markOverlaps :: [Area] -> ClothMap
markOverlaps areaList = foldl mark Map.empty areaList where
    updateMap markMap point =
        Map.alter update point markMap where
            update Nothing = Just 0
            update _ = Just 1
    mark markMap (Area {..}) = foldl updateMap markMap coords where
        coords = concatMap (\x -> fmap (x,) [areaY..(areaY + areaHeight - 1)]) [areaX..(areaX + areaWidth - 1)]

parseAndProcessAreas :: ([Area] -> Int) -> T.Text -> Int
parseAndProcessAreas process input =
    case parse parser "" input of
        Right areaList -> process areaList
        Left err -> trace (show err) 0
        
findOverlapping :: T.Text -> Int
findOverlapping = parseAndProcessAreas $ Map.foldr' (+) 0 . markOverlaps