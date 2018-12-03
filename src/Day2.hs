module Day2
    ( checksum
    ) where

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

checksum :: T.Text -> Int
checksum input =
    let
        updateMap seen el =
            Map.alter alter el seen where
                alter (Just value) = Just $ value + 1
                alter Nothing = Just 1
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
