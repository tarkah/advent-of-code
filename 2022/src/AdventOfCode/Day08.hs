module AdventOfCode.Day08
    ( run
    ) where

import Data.Char (digitToInt)
import Data.List (foldl', find)
import qualified Data.Set as Set


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . length . visibleTrees . runSurvey . parseForest

        solve2 =
            show . maximum . map product . scenicScores . parseForest

        visibleTrees =
            foldl1 Set.union . map _found . flip map [ Left', Right', Up, Down ]

        treeScore forest tree =
            map (scenicScore forest tree) [ Left', Right', Up, Down ]

        scenicScores forest =
            map (takeWhile (> 0)) . map (treeScore forest) . _trees $ forest


type Line = Int


data Tree
    = Tree { _id :: Int, _size :: Int }
    deriving (Show, Ord, Eq)


data Forest
    = Forest { _width :: Int, _height :: Int, _trees :: [Tree] }
    deriving Show


data Survey
    = Survey { _heights :: [Int], _found :: Set.Set Tree }
    deriving Show


data Direction
    = Left'
    | Right'
    | Up
    | Down
    deriving Show


scenicScore :: Forest -> Tree -> Direction -> Int
scenicScore forest tree direction =
    let
        prepared =
            prepareForest direction forest

        line =
            maybe 0 id . fmap fst . find ((== (_id tree)) . _id . snd) $ prepared

        trees =
            map snd . filter ((== line) . fst) $ prepared

        isAfter =
            case direction of
                Left' ->
                    (< (_id tree))

                Right' ->
                    (> (_id tree))

                Up ->
                    (< (_id tree))

                Down ->
                    (> (_id tree))

        lineOfSight =
            filter (isAfter . _id) $ trees

        scenic =
            scenicTrees tree lineOfSight
    in
    length scenic


scenicTrees :: Tree -> [Tree] -> [Tree]
scenicTrees tree (x : xs) =
    if (>= (_size tree)) (_size x) then
        [ x ]

    else
        x : scenicTrees tree xs
scenicTrees _ _ =
    []


runSurvey :: Forest -> Direction -> Survey
runSurvey forest direction =
    foldl' (\survey ( i, tree ) -> surveyTree survey i tree) (newSurvery direction forest) $ prepareForest direction forest


surveyTree :: Survey -> Int -> Tree -> Survey
surveyTree (Survey heights found) line tree@(Tree _ size) =
    let
        max' =
            heights !! line

        updateMax heights' size' =
            (\( x, xs ) -> x ++ size' : tail xs) $ splitAt line heights'
    in
    if size > max' then
        Survey (updateMax heights size) (Set.insert tree found)

    else
        Survey heights found


prepareForest :: Direction -> Forest -> [( Line, Tree )]
prepareForest direction (Forest width _ trees) =
    let
        withLine =
            (\( i, tree ) -> ( surveyLine direction width i, tree ))
    in
    map withLine . zip [0..] . orderedTrees direction $ trees


orderedTrees :: Direction -> [Tree] -> [Tree]
orderedTrees Left' =
    reverse
orderedTrees Up =
    reverse
orderedTrees _ =
    id


surveyLine :: Direction -> Int -> Int -> Line
surveyLine Left' width =
    (`div` width)
surveyLine Right' width =
    (`div` width)
surveyLine Up width =
    (`mod` width)
surveyLine Down width =
    (`mod` width)


newSurvery :: Direction -> Forest -> Survey
newSurvery direction forest =
    let
        size =
            crossSize direction forest

        heights =
            take size $ repeat (-1)
    in
    Survey heights Set.empty


crossSize :: Direction -> Forest -> Int
crossSize Left' =
    _height
crossSize Right' =
    _height
crossSize Up =
    _width
crossSize Down =
    _width


parseForest :: String -> Forest
parseForest s =
    let
        rows =
            lines s

        width =
            length . head $ rows

        height =
            length rows

        trees =
            map (uncurry Tree) . zip [0..] . map digitToInt . concat $ rows
    in
    Forest width height trees
