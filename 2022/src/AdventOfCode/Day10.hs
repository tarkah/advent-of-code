module AdventOfCode.Day10
    ( run
    ) where

import Data.Maybe (catMaybes)


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . sum . map strength . filtered . runCpu . instructions

        solve2 =
            (\(Screen w _ pixels) -> display w pixels) . render (newScreen 40 6) . runCpu . instructions

        instructions =
            catMaybes . map parseInstruction . lines

        ticks = take 6 $ [20,60..]
        
        filtered processed =
            map ((!!) processed . subtract 1) $ ticks


data Instruction
    = Noop
    | AddX Int
    deriving Show


type Register = Int


data State
    = State { _cycle :: Int, _rX :: Register }
    deriving Show


data Screen
    = Screen { _w :: Int, _h :: Int, _pixels :: [Bool] }
    deriving Show


display :: Int -> [Bool] -> String
display w pixels =
    case (splitAt w pixels) of
        ( [], [] ) ->
            []

        ( row, xs ) ->
            map (\p -> if p then '#' else '.') row ++ '\n' : display w xs


render :: Screen -> [State] -> Screen
render screen@(Screen w h _) ((State cycle' rX) : xs) =
    render (Screen w h (renderPixel screen cycle' rX)) xs
render screen _ =
    screen


renderPixel :: Screen -> Int -> Int -> [Bool]
renderPixel (Screen w _ pixels) cycle' rX =
    let
        index =
            subtract 1 cycle'

        pos =
            index `mod` w

        pixel =
            spriteHit pos rX
    in
    case (splitAt index pixels) of
        ( xs, (_ : ys) ) ->
            xs ++ (pixel : ys)

        ( _, [] ) ->
            pixels


spriteHit :: Int -> Int -> Bool
spriteHit pos rX =
    rX >= subtract 1 pos && rX <= (+ 1) pos


newScreen :: Int -> Int -> Screen
newScreen w h =
    Screen w h (take (w * h) $ repeat False)


strength :: State -> Int
strength (State cycle' rX) =
    cycle' * rX


runCpu :: [Instruction] -> [State]
runCpu =
    scanl decode (State 1 1) . padCycles


decode :: State -> Instruction -> State
decode (State cycle' rX) Noop =
    State ((+ 1) cycle') rX
decode (State cycle' rX) (AddX i) =
    State ((+ 1) cycle') ((+ i) rX)


padCycles :: [Instruction] -> [Instruction]
padCycles ((Noop) : xs) =
    Noop : padCycles xs
padCycles (int@(AddX _) : xs) =
    Noop : int : padCycles xs
padCycles _ =
    []


parseInstruction :: String -> Maybe Instruction
parseInstruction ('n' : 'o' : 'o' : 'p' : _) =
    Just Noop
parseInstruction ('a' : 'd' : 'd' : 'x' : ' ' : xs) =
    Just $ AddX (read xs)
parseInstruction _ =
    Nothing
