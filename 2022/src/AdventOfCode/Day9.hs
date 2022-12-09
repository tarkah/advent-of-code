module AdventOfCode.Day9
    ( run
    ) where

import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Ord (clamp)


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . process 2

        solve2 =
            show . process 10

        process n =
            length . nub . map (head . reverse) . map _points . processMoves (newState n) . parseMoves

        parseMoves =
            catMaybes . map parseMove . lines


data Direction
    = U
    | D
    | L
    | R
    deriving Show


data Move
    = Move { _direction :: Direction, _n :: Int }
    deriving Show


data Point
    = Point { x :: Int, y :: Int }
    deriving (Show, Eq)


data State
    = State { _points :: [Point] }
    deriving Show


processMoves :: State -> [Move] -> [State]
processMoves state ((Move direction n) : xs) =
    let
        updated =
            scanr id state . take n . repeat $ (processMove direction)
    in
    (reverse updated) ++ processMoves (head updated) xs
processMoves state _ =
    [ state ]


processMove :: Direction -> State -> State
processMove direction (State points) =
    let
        newHead =
            updateHead direction (head points)

        diff parent knot' =
            componentWise (-) parent knot'

        newKnots =
            tail $ scanl (\parent knot' -> updateNot knot' (diff parent knot')) newHead (tail points)
    in
    State (newHead : newKnots)


updateHead :: Direction -> Point -> Point
updateHead U (Point x' y') =
    Point x' (y' + 1)
updateHead D (Point x' y') =
    Point x' (y' - 1)
updateHead L (Point x' y') =
    Point (x' - 1) y'
updateHead R (Point x' y') =
    Point (x' + 1) y'


updateNot :: Point -> Point -> Point
updateNot tail' diff@(Point x' y') =
    let
        between low high i =
            ((low) <= i && i <= high)
    in
    if ((between (-1) 1 x') && (between (-1) 1 y')) then
        tail'

    else
        componentWise (+) tail' (clamp' ( (-1), 1 ) diff)


componentWise :: (Int -> Int -> Int) -> Point -> Point -> Point
componentWise f a b =
    Point (f (x a) (x b)) (f (y a) (y b))


clamp' :: ( Int, Int ) -> Point -> Point
clamp' ( low, high ) (Point x' y') =
    Point (clamp ( low, high ) x') (clamp ( low, high ) y')


newPoint :: Point
newPoint =
    Point 0 0


newState :: Int -> State
newState knots =
    State . take knots . repeat $ newPoint


parseMove :: String -> Maybe Move
parseMove ('U' : ' ' : xs) =
    Just $ Move U (read xs)
parseMove ('D' : ' ' : xs) =
    Just $ Move D (read xs)
parseMove ('L' : ' ' : xs) =
    Just $ Move L (read xs)
parseMove ('R' : ' ' : xs) =
    Just $ Move R (read xs)
parseMove _ =
    Nothing
