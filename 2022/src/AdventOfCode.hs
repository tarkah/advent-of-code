module AdventOfCode
    ( run
    ) where

import qualified AdventOfCode.Day1 as Day1
import qualified AdventOfCode.Day2 as Day2
import qualified AdventOfCode.Day3 as Day3
import qualified AdventOfCode.Day4 as Day4
import qualified AdventOfCode.Day5 as Day5


run :: IO ()
run = do
    inputs <- mapM getInput days
    putStr . unlines . map (uncurry runDay) $ zip days inputs


data Day
    = Day { _number :: Int, _run :: Run }


type Run = String -> ( String, String )


days :: [Day]
days =
    map (uncurry Day) $ zip [1..] [ Day1.run, Day2.run, Day3.run, Day4.run, Day5.run ]


runDay :: Day -> String -> String
runDay (Day number _run) input =
    unlines [ "--- Day " ++ show number ++ " ---", part1, part2 ]
    where
        ( part1, part2 ) =
            _run input


getInput :: Day -> IO String
getInput (Day number _) =
    readFile $ "input/Day" ++ show number ++ ".txt"
