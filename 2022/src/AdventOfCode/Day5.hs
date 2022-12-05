module AdventOfCode.Day5
    ( run
    ) where

import Data.List (transpose)
import Data.Maybe (catMaybes)


run :: String -> ( String, String )
run input =
    ( solve1, solve2 )
    where
        solve1 =
            topCrates $ processMoves ship CM9000 moves

        solve2 =
            topCrates $ processMoves ship CM9001 moves

        ship =
            parseShip input

        moves =
            parseMoves input

        topCrates =
            map head


type Crate =
    Char


type Stack = [Crate]


type Ship = [Stack]


data Move
    = Move { _num :: Int, _from :: Int, _to :: Int }
    deriving Show


data Mover
    = CM9000
    | CM9001


moveCrates :: Ship -> Int -> Int -> Int -> Ship
moveCrates ship from to n =
    [
        if i == to then
            (fst $ lift n (getStack ship from)) ++ getStack ship to

        else
            if i == from then
                snd $ lift n (getStack ship from)

            else
                stack
        | ( i, stack ) <- zip [1..] ship
    ]
    where
        getStack ship' i =
            ship' !! (i - 1)

        lift =
            splitAt


processMoves :: Ship -> Mover -> [Move] -> Ship
processMoves ship mover (move : moves) =
    processMoves (process ship move) mover moves
    where
        process ship' (Move n f t) =
            case mover of
                CM9000 ->
                    foldl (\ship'' _ -> moveCrates ship'' f t 1) ship' [1..n]

                CM9001 ->
                    moveCrates ship' f t n
processMoves ship _ _ =
    ship


parseShip :: String -> Ship
parseShip =
    map catMaybes . transpose . rows . top
    where
        top =
            fst . break (== "") . lines

        rows xs =
            case (reverse xs) of
                (_ : b) ->
                    map parseRow $ reverse b

                _ ->
                    []


parseRow :: String -> [Maybe Crate]
parseRow (' ' : ' ' : ' ' : []) =
    [ Nothing ]
parseRow (_ : c : _ : []) =
    [ Just $ c ]
parseRow (' ' : ' ' : ' ' : ' ' : xs) =
    Nothing : parseRow xs
parseRow (_ : c : _ : ' ' : xs) =
    Just c : parseRow xs
parseRow _ =
    []


parseMoves :: String -> [Move]
parseMoves =
    catMaybes . map parseMove . bottom
    where
        bottom =
            tail . snd . break (== "") . lines


parseMove :: String -> Maybe Move
parseMove s =
    case (split ' ' s) of
        ("move" : n : "from" : f : "to" : t : []) ->
            Just $ Move (read n) (read f) (read t)

        _ ->
            Nothing


split :: Char -> String -> [String]
split p s =
    case (break (== p) s) of
        ( a, (_ : b) ) ->
            a : split p b

        ( a, [] ) ->
            [ a ]
