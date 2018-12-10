module Day2
    ( checksum
    , offByOne
    ) where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Foldable (find)

checksum :: T.Text -> Int
checksum input =
    let
        updateMap seen el =
            Map.alter update el seen where
                update (Just value) = Just $ value + 1
                update Nothing = Just 1
        count ids =
            let
                go (double, triple) id =
                    let
                        chars = T.foldl updateMap Map.empty id
                        counts = Set.fromList ( Map.elems chars )
                    in
                        ( double + fromEnum ( Set.member 2 counts )
                        , triple + fromEnum ( Set.member 3 counts ) )
            in
                foldl go (0, 0) ids
        (double, triple) = count $ T.lines input
    in
        double * triple

offByOne :: T.Text -> T.Text
offByOne input = findOffByOne ids ids where
    ids = T.lines input
    gatherSameLetters text (c1, c2) =
        if c1 == c2
        then T.snoc text c1
        else text
    common w1 w2 = foldl gatherSameLetters "" (T.zip w1 w2)
    findOffByOne [] _ = "Not found"
    findOffByOne (w1:rest) ids =
        case find (\w2 -> T.length w1 - T.length ( common w1 w2 ) == 1) ids of
            Just w2 -> common w1 w2
            Nothing -> findOffByOne rest ids