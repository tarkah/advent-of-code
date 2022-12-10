module AdventOfCode.Day01
    ( run
    ) where

import Data.List (sort)


run :: String -> ( String, String )
run input =
    ( show max', show topThree )
    where
        groupings =
            map (map toInt) $ groups $ lines input

        sums =
            map sum groupings

        max' =
            maximum sums

        topThree =
            sum $ take 3 $ reverse $ sort sums


groups :: [String] -> [[String]]
groups arr =
    case break (== "") arr of
        ( a, [] ) ->
            [ a ]

        ( a, b ) ->
            [ a ] ++ (groups $ tail b)


toInt :: String -> Int
toInt =
    read
