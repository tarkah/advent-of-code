module AdventOfCode
    ( run 
    ) where

import qualified AdventOfCode.Day1 as Day1

run :: IO ()
run = do
    inputs <- mapM getInput days
    _ <- mapM (uncurry displayDay) $ zip days inputs
    return ()

data Day = Day { _number :: Int, _run :: String -> String }

newDay :: Int -> (String -> String) -> Day
newDay _number _run = Day _number _run

days :: [Day]
days = map (uncurry newDay) $ zip [1..] [Day1.run]

displayDay :: Day -> String -> IO ()
displayDay (Day number _run) input = do
    putStrLn $ "--- Day " ++ show number ++ " ---"
    putStrLn result
    putStrLn ""
    where result = _run input
    
getInput :: Day -> IO String
getInput (Day number _) = readFile $ "input/Day" ++ show number ++ ".txt"
