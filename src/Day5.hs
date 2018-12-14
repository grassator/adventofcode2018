module Day5
( react
, shortest
) where

import qualified Data.Text as T
import Data.Char (toUpper)
import qualified Data.Set as Set

canReact :: Char -> Char -> Bool
canReact x y = x /= y && toUpper x == toUpper y

react' :: String -> String -> Int
react' [] (y:ys) = react' [y] ys
react' x [] = length x
react' (x:xs) (y:ys) =
    if canReact x y
    then react' xs ys
    else react' (y:x:xs) ys

react :: T.Text -> Int
react = react' [] . T.unpack where

shortest :: T.Text -> Int
shortest input = minimum polymers where
    chars = T.unpack input
    uniqueTypes = Set.toList $ Set.fromList $ toUpper <$> chars
    polymers = go <$> uniqueTypes where
        go typ = react' [] $ filter (\c -> toUpper c /= typ) chars