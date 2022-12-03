module AdventOfCode.Day2
    ( run
    ) where

import Data.Maybe (catMaybes)


run :: String -> String
run input =
    (show $ total matches) ++ "\n" ++ (show $ total planMatches)
    where
        matches =
            parseMatches input

        planMatches =
            map planMatch . parsePlans $ input

        scores =
            map scoreMatch

        total =
            sum . scores


data Move
    = Rock
    | Paper
    | Scissors
    deriving (Show, Eq)


data Match
    = Match { mine :: Move, opponent :: Move }
    deriving Show


data Outcome
    = Win
    | Lose
    | Draw
    deriving Show


data Plan
    = Plan { _opponent :: Move, _outcome :: Outcome }
    deriving Show


parseMove :: Char -> Maybe Move
parseMove 'A' =
    Just Rock
parseMove 'X' =
    Just Rock
parseMove 'B' =
    Just Paper
parseMove 'Y' =
    Just Paper
parseMove 'C' =
    Just Scissors
parseMove 'Z' =
    Just Scissors
parseMove _ =
    Nothing


parseMatch :: String -> Maybe Match
parseMatch (a : ' ' : b : []) =
    case ( parseMove a, parseMove b ) of
        ( Just o, Just m ) ->
            Just $ Match m o

        _ ->
            Nothing
parseMatch _ =
    Nothing


parseOutcome :: Char -> Maybe Outcome
parseOutcome 'X' =
    Just Lose
parseOutcome 'Y' =
    Just Draw
parseOutcome 'Z' =
    Just Win
parseOutcome _ =
    Nothing


parsePlan :: String -> Maybe Plan
parsePlan (a : ' ' : b : []) =
    case ( parseMove a, parseOutcome b ) of
        ( Just _opponent, Just _outcome ) ->
            Just $ Plan _opponent _outcome

        _ ->
            Nothing
parsePlan _ =
    Nothing


parseMatches :: String -> [Match]
parseMatches input =
    catMaybes $ map parseMatch $ lines input


parsePlans :: String -> [Plan]
parsePlans input =
    catMaybes $ map parsePlan $ lines input


outcome :: Match -> Outcome
outcome (Match m o) =
    if m == o then Draw else if beats o == m then Win else Lose


beats :: Move -> Move
beats Rock =
    Paper
beats Paper =
    Scissors
beats Scissors =
    Rock


loses :: Move -> Move
loses Rock =
    Scissors
loses Paper =
    Rock
loses Scissors =
    Paper


scoreMove :: Move -> Int
scoreMove Rock =
    1
scoreMove Paper =
    2
scoreMove Scissors =
    3


scoreOutcome :: Outcome -> Int
scoreOutcome Win =
    6
scoreOutcome Draw =
    3
scoreOutcome Lose =
    0


scoreMatch :: Match -> Int
scoreMatch match =
    (scoreMove . mine $ match) + (scoreOutcome . outcome $ match)


planMatch :: Plan -> Match
planMatch (Plan _opponent Win) =
    Match (beats _opponent) _opponent
planMatch (Plan _opponent Lose) =
    Match (loses _opponent) _opponent
planMatch (Plan _opponent Draw) =
    Match _opponent _opponent
