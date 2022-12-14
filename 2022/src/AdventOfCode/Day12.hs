module AdventOfCode.Day12
    ( run
    ) where

import Data.List (findIndex, elemIndex, sortOn)
import Data.Maybe (isJust)
import qualified Data.Set as Set


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . subtract 1 . length . uncurry shortestPath . parse

        solve2 =
            show . fromLowest . parse

        fromLowest ( grid, _ ) =
            minimum . map (subtract 1 . length) . map (shortestPath grid) . filter (isStarting grid) $ concat grid

        isStarting grid n =
            (nodeChar n) == 'a' && (isJust $ elemIndex 'b' $ map nodeChar $ map (position_to_node grid) (children n))


type Position = ( Int, Int )


type Grid = [[Node]]


type Path = [Node]


data Node
    = Start Position [Position]
    | Segment Position Char [Position]
    | End Position
    deriving (Eq, Ord)


instance Show Node where
    show (Start _ _) =
        "Start"
    show (Segment p c _) =
        show c ++ show p
    show (End _) =
        "End"


hasEnd :: Path -> Bool
hasEnd [] =
    False
hasEnd ((End _) : _) =
    True
hasEnd (_ : xs) =
    hasEnd xs


shortestPath :: Grid -> Node -> Path
shortestPath grid start =
    head . sortOn length $ shortestPath' grid [ [ start ] ]


shortestPath' :: Grid -> [Path] -> [Path]
shortestPath' grid paths =
    let
        new =
            step grid paths

        filtered =
            filter hasEnd new
    in
    if length filtered > 0 then filtered else shortestPath' grid new


step :: Grid -> [Path] -> [Path]
step grid paths =
    fst . head . scanr mapBranch ( [], nodeSet paths ) $ paths
    where
        nodeSet =
            Set.fromList . concat

        mapBranch path ( newPaths, all' ) =
            (\branched -> ( branched ++ newPaths, Set.union all' (nodeSet branched) )) (branch path all')

        notVisited =
            flip Set.notMember

        filteredChildren paths' =
            filter (notVisited paths') . map (position_to_node grid) . children

        branch (x : xs) paths' =
            case filteredChildren paths' x of
                [] ->
                    [ x : xs ]

                xs' ->
                    map (\x' -> x' : x : xs) xs'
        branch [] _ =
            []


position_to_node :: Grid -> Position -> Node
position_to_node grid ( x, y ) =
    (grid !! y) !! x


children :: Node -> [Position]
children (Start _ c) =
    c
children (Segment _ _ c) =
    c
children (End _) =
    []


nodeChar :: Node -> Char
nodeChar (Start _ _) =
    'a'
nodeChar (Segment _ c _) =
    c
nodeChar (End _) =
    'z'


sizeChar :: Char -> Int
sizeChar 'S' =
    sizeChar 'a'
sizeChar 'E' =
    sizeChar 'z'
sizeChar c =
    (+ 1) $ maybe 0 id $ elemIndex c ['a'..'z']


parse :: String -> ( Grid, Node )
parse s =
    let
        rows =
            lines s

        width =
            length . head $ rows

        height =
            length rows

        startIndex =
            maybe 0 id . findIndex (== 'S') $ filter (/= '\n') s

        nodes =
            map (map (parseNode rows)) $
                [ [ ( x, y ) | x <- [0..(subtract 1 width)] ] | y <- [0..(subtract 1 height)] ]

        start =
            (nodes !! (startIndex `div` width)) !! (startIndex `mod` width)
    in
    ( nodes, start )


parseNode :: [String] -> Position -> Node
parseNode rows pos@( x, y ) =
    let
        char x' y' =
            (rows !! y') !! x'

        width =
            length . head $ rows

        height =
            length rows

        validStep ( x', y' ) =
            x' >= 0
                && y' >= 0
                    && x' < width
                        && y' < height
                            && (sizeChar $ char x' y') - (sizeChar $ char x y) <= 1

        neighbors =
            [
                ( x', y' )
                | ( x', y' ) <-
                    [ ( subtract 1 x, y )
                    , ( (+ 1) x, y )
                    , ( x, subtract 1 y )
                    , ( x, (+ 1) y )
                    ]
                , validStep ( x', y' )
            ]
    in
    case char x y of
        'E' ->
            End pos

        'S' ->
            Start pos neighbors

        c' ->
            Segment pos c' neighbors
