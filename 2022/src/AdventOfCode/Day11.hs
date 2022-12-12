module AdventOfCode.Day11
    ( run
    ) where

import Data.Char (isDigit)
import Data.List (findIndex, transpose, sort)
import Text.ParserCombinators.ReadP


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . monkeyBusiness . take 20 . play . parse

        solve2 =
            show . monkeyBusiness . take 10000 . playV2 . parse

        monkeyBusiness =
            product . take 2 . reverse . sort . map sum . transpose . map _inspections


data Monkey
    = Monkey { _id :: Int, _items :: [Int], _operation :: Operation, _test :: Test }
    deriving Show


data Operation
    = Mul Int
    | Add Int
    | Squared
    deriving Show


data Test
    = Test { _divisible :: Int, _true :: Int, _false :: Int }
    deriving Show


data Round
    = Round { _end :: [Monkey], _inspections :: [Int] }
    deriving Show


data Version
    = V1
    | V2 Int
    deriving Show


play :: [Monkey] -> [Round]
play ms =
    let
        round' =
            playRound V1 ms
    in
    round' : play (_end round')


playV2 :: [Monkey] -> [Round]
playV2 ms =
    let
        v =
            V2 $ product . map (_divisible . _test) $ ms

        round' =
            playRound v ms
    in
    round' : playV2 (_end round')


playRound :: Version -> [Monkey] -> Round
playRound v start =
    let
        turns =
            scanl (\xs i -> processTurn v (xs !! i) xs) start . map _id $ start

        end =
            head . reverse $ turns

        mask =
            [0..(subtract 1 (length start))]

        inspections =
            map (length . _items) . map (uncurry (!!)) . zip turns $ mask
    in
    Round end inspections


processTurn :: Version -> Monkey -> [Monkey] -> [Monkey]
processTurn _ (Monkey _ [] _ _) ms =
    ms
processTurn v (Monkey id' (i : xs) o t) ms =
    let
        updated =
            Monkey id' xs o t

        removed =
            case (flip splitAt ms . maybe 0 id . findIndex ((== id') . _id) $ ms) of
                ( xs', [] ) ->
                    xs'

                ( xs', (_ : ys') ) ->
                    xs' ++ updated : ys'
    in
    processTurn v updated (throwItem v o t i removed)


throwItem :: Version -> Operation -> Test -> Int -> [Monkey] -> [Monkey]
throwItem v o (Test d t f) i ms =
    let
        worry' =
            case v of
                V1 ->
                    worry o i `div` 3

                V2 x ->
                    worry o i `mod` x

        to =
            if (worry' `mod` d == 0) then t else f
    in
    case (splitAt to ms) of
        ( xs, [] ) ->
            xs

        ( xs, ((Monkey id' items o' t') : ys) ) ->
            xs ++ (Monkey id' (items ++ [ worry' ]) o' t') : ys


worry :: Operation -> Int -> Int
worry (Add x) i =
    i + x
worry (Mul x) i =
    i * x
worry Squared i =
    i * i


parse :: String -> [Monkey]
parse s =
    case readP_to_S parseMonkeys s of
        [] ->
            []

        (( xs, _ ) : _) ->
            xs


parseMonkeys :: ReadP [Monkey]
parseMonkeys = do
    monkeys <- sepBy1 parseMonkey (char '\n')
    eof
    return monkeys


parseMonkey :: ReadP Monkey
parseMonkey = do
    discardTill (string " ")
    id' <- number
    discardTill (string ":\n")
    items <- parseItems
    operation <- parseOperation
    test <- parseTest
    return $ Monkey id' items operation test


parseItems :: ReadP [Int]
parseItems = do
    discardTill (string ": ")
    items <- sepBy1 number (string ", ")
    skipNewline
    return items


parseOperation :: ReadP Operation
parseOperation = do
    discardTill (string "old ")
    op <- choice [ char '*', char '+' ]
    skipSpaces

    operation <-
        choice
            [ number
                >>= (\i ->
                    case op of
                        '*' ->
                            return $ Mul i

                        '+' ->
                            return $ Add i

                        _ ->
                            pfail
                )
            , string "old" >> return Squared
            ]

    skipNewline
    return operation


parseTest :: ReadP Test
parseTest = do
    discardTill (string "by ")
    divisible <- number
    skipNewline
    discardTill (string "monkey ")
    true <- number
    skipNewline
    discardTill (string "monkey ")
    false <- number
    skipNewline
    return $ Test divisible true false


number :: ReadP Int
number =
    (fmap read . munch1) isDigit


skipNewline :: ReadP ()
skipNewline =
    char '\n' >> return ()


discardTill :: ReadP a -> ReadP ()
discardTill a =
    manyTill get a >> return ()
