module Day5
( react
) where

import qualified Data.Text as T
import Data.Char (toUpper)

canReact :: Char -> Char -> Bool
canReact x y = x /= y && toUpper x == toUpper y

react :: T.Text -> Int
react = go [] . T.unpack where
    go [] (y:ys) = go [y] ys
    go x [] = length x
    go (x:xs) (y:ys) =
        if canReact x y
        then go xs ys
        else go (y:x:xs) ys
