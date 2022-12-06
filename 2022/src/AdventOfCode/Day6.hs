module AdventOfCode.Day6
    ( run
    ) where

import Data.List (nub)


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . markerIndex Packet

        solve2 =
            show . markerIndex Message


data Marker
    = Packet
    | Message


markerIndex :: Marker -> String -> Int
markerIndex marker s =
    (-) (length s) (length $ extractAfter marker s)


extractAfter :: Marker -> String -> String
extractAfter Packet =
    search 4
extractAfter Message =
    search 14


search :: Eq a => Int -> [a] -> [a]
search n =
    search' []
    where
        search' magic s =
            if (== n) (length . nub $ magic) then
                s

            else
                search' (head s : (fst $ splitAt (n - 1) magic)) (tail s)
