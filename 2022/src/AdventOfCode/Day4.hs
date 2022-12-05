module AdventOfCode.Day4
    ( run
    ) where

import qualified Data.Set as Set


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . length . filter pairHasSubset . parseAssignments

        solve2 =
            show . length . filter pairHasOverlap . parseAssignments


type Section = Set.Set Int


type Pair = ( Section, Section )


pairHasOverlap :: Pair -> Bool
pairHasOverlap ( a, b ) =
    not $ Set.disjoint a b


pairHasSubset :: Pair -> Bool
pairHasSubset ( a, b ) =
    Set.isSubsetOf a b || Set.isSubsetOf b a


parsePair :: String -> ( Section, Section )
parsePair =
    mapTuple parseSection . split
    where
        split =
            (\( a, b ) -> ( a, tail b )) . break (== ',')


parseSection :: String -> Section
parseSection s =
    Set.fromList [a..b]
    where
        ( a, b ) =
            case (break (== '-') s) of
                ( a', ('-' : b') ) ->
                    ( read a', read b' )

                _ ->
                    ( 0, 0 )


parseAssignments :: String -> [Pair]
parseAssignments =
    map parsePair . lines


mapTuple :: (a -> b) -> ( a, a ) -> ( b, b )
mapTuple f ( a, b ) =
    ( f a, f b )
