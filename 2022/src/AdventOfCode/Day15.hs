module AdventOfCode.Day15
    ( run
    ) where

import Data.Char (isDigit)
import Data.List (nub)
import Text.ParserCombinators.ReadP


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . length . impossibleBeaconsRow 2000000 . grid . parse

        solve2 =
            show . tuningFrequency 4000000 . grid . parse

        nonBeacons beacons =
            filter (not . (`elem` beacons))

        impossibleBeaconsRow y grid'@(Grid sensors beacons _ _) =
            nonBeacons beacons . map snd . filter (id . fst) . map (withinRange sensors) $ (row y grid')

        tuningFrequency size (Grid sensors beacons _ _) =
            (\(Point x y) -> x * 4000000 + y)
                . head
                    . map snd
                        . filter (not . id . fst)
                            . map (withinRange sensors)
                                . filter (\(Point x y) -> x >= 0 && x <= size && y >= 0 && y <= size) . nonBeacons beacons . nub . concat . map surroundingPoints $
                sensors


data Point
    = Point { _x :: Int, _y :: Int }
    deriving (Show, Eq, Ord)


data Sensor
    = Sensor { _point :: Point, _range :: Int }
    deriving Show


type Beacon = Point


data Grid
    = Grid { _sensors :: [Sensor], _beacons :: [Beacon], _min :: Point, _max :: Point }
    deriving Show


withinRange :: [Sensor] -> Point -> ( Bool, Point )
withinRange ((Sensor s range') : xs) point =
    if distance s point <= range' then
        ( True, point )

    else
        withinRange xs point
withinRange [] point =
    ( False, point )


surroundingPoints :: Sensor -> [Point]
surroundingPoints (Sensor point range') =
    concat . map (build point (range' + 1)) $ [0..(range' + 1)]
    where
        build (Point x y) range'' offsetX =
            let
                offsetY =
                    range'' - offsetX
            in
            [ (Point (x + offsetX) (y + offsetY)), (Point (subtract offsetX x) (subtract offsetY y)) ]


row :: Int -> Grid -> [Point]
row y (Grid _ _ (Point minX _) (Point maxX _)) =
    map (\x -> Point x y) [minX..maxX]


grid :: [( Point, Point )] -> Grid
grid points =
    let
        sensors =
            map sensor points

        maxRange =
            maximum . map (_range) $ sensors

        flattened =
            concat . map (\( a, b ) -> [ a, b ]) $ points

        minX =
            (subtract maxRange) . minimum . map _x $ flattened

        minY =
            (subtract maxRange) . minimum . map _y $ flattened

        maxX =
            (+ maxRange) . maximum . map _x $ flattened

        maxY =
            (+ maxRange) . maximum . map _y $ flattened

        beacons =
            map snd points
    in
    Grid sensors beacons (Point minX minY) (Point maxX maxY)


sensor :: ( Point, Point ) -> Sensor
sensor ( s, b ) =
    Sensor s (distance s b)


distance :: Point -> Point -> Int
distance (Point ax ay) (Point bx by) =
    abs (ax - bx) + abs (ay - by)


parse :: String -> [( Point, Point )]
parse s =
    case readP_to_S (manyTill parseLine eof) s of
        (( x, _ ) : _) ->
            x

        _ ->
            []


parseLine :: ReadP ( Point, Point )
parseLine = do
    sensor' <- preceeded (string "Sensor at ") parsePoint
    beacon <- preceeded (string ": closest beacon is at ") parsePoint
    _ <- char '\n'
    return ( sensor', beacon )


parsePoint :: ReadP Point
parsePoint = do
    x <- preceeded (string "x=") number
    _ <- string ", "
    y <- preceeded (string "y=") number
    return $ Point x y


number :: ReadP Int
number =
    (fmap read . munch1) $ (\c -> c == '-' || isDigit c)


preceeded :: ReadP a -> ReadP b -> ReadP b
preceeded a b =
    a *> b
