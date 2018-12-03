module Day1
    ( calibrate
    , findRepeating
    ) where

import qualified Data.Text as T
import qualified Data.Set as Set

parse :: T.Text -> [Int]
parse input = 
    let
        parseInt = ( read . T.unpack . (T.replace "+" "") )
    in
        parseInt <$> ( T.splitOn "\n" input )

calibrate :: T.Text -> Int
calibrate = ( foldl (+) 0 ) . parse

findRepeating :: T.Text -> Int
findRepeating =
    let
        find prev seen (el:rest) =
            let
                current = prev + el
                found = Set.member current seen
            in
                if found
                then current
                else find current ( Set.insert current seen ) rest
    in
        ( find 0 Set.empty ) . cycle . parse
