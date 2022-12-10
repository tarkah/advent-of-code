module AdventOfCode.Day03
    ( run
    ) where

import Data.Char (isAlpha)
import Data.List (elemIndex)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . sum . priorities . intersections . parseSacks

        solve2 =
            show . sum . priorities . groupIntersections . groupSacks . parseSacks

        intersections =
            map (uncurry Set.intersection)

        groupIntersections =
            map groupIntersection

        priorities =
            map (sum . Set.map priority)


newtype Alpha
    = Alpha Char
    deriving (Show, Ord, Eq)


type Compartment = Set.Set Alpha


type Sack = ( Compartment, Compartment )


type Group = ( Sack, Sack, Sack )


newAlpha :: Char -> Maybe Alpha
newAlpha c =
    if isAlpha c then Just $ Alpha c else Nothing


priority :: Alpha -> Int
priority (Alpha a) =
    (+ 1) . maybe 0 id $ elemIndex a $ ['a'..'z'] ++ ['A'..'Z']


parseSacks :: String -> [Sack]
parseSacks =
    map parseLine . lines
    where
        parseLine =
            mapTuple compartment . splitMid . catMaybes . map newAlpha

        splitMid =
            \list -> splitAt ((length list) `div` 2) list

        compartment =
            Set.fromList


groupSacks :: [Sack] -> [Group]
groupSacks (a : b : c : d) =
    [ ( a, b, c ) ] ++ groupSacks d
groupSacks _ =
    []


sackSet :: Sack -> Set.Set Alpha
sackSet ( a, b ) =
    Set.union a b


groupIntersection :: Group -> Set.Set Alpha
groupIntersection group =
    Set.intersection a $ Set.intersection b c
    where
        ( a, b, c ) =
            mapTuple3 sackSet group


mapTuple :: (a -> b) -> ( a, a ) -> ( b, b )
mapTuple f ( one, two ) =
    ( f one, f two )


mapTuple3 :: (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTuple3 f ( one, two, three ) =
    ( f one, f two, f three )
