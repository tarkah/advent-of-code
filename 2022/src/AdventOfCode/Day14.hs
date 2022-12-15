module AdventOfCode.Day14
    ( run
    ) where

import Data.Char (isDigit)
import Data.List (sort, nub)
import Text.ParserCombinators.ReadP


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . length . filter isSand . _material . simulate False . parse

        solve2 =
            show . (+ 1) . length . filter isSand . _material . simulate True . parse


data Direction
    = D
    | DL
    | DR


data Point
    = Point { _x :: Int, _y :: Int }
    deriving (Eq, Ord)


type Path = [Point]


data Material
    = Rock Point
    | Sand Point
    deriving (Eq, Ord)


data Cave
    = Cave { _bl :: Point, _br :: Point, _material :: [Material] }
    deriving Show


data Floor
    = Infinite Int
    | Platform Point Point


instance Show Point where
    show (Point x y) =
        "(" ++ show x ++ "," ++ show y ++ ")"


instance Show Material where
    show (Rock p) =
        "R" ++ show p
    show (Sand p) =
        "S" ++ show p


simulate :: Bool -> Cave -> Cave
simulate useFloor cave@(Cave bl br points) =
    let
        floor' =
            if useFloor then Infinite $ (_y bl) + 2 else Platform bl br

        fallenSand =
            fall (map point points) floor' sand
    in
    if finish floor' fallenSand then
        cave

    else
        simulate useFloor (Cave bl br (Sand fallenSand : points))


finish :: Floor -> Point -> Bool
finish (Platform bl br) (Point x _) =
    x < (_x bl) || x > (_x br)
finish (Infinite _) x =
    x == sand


fall :: [Point] -> Floor -> Point -> Point
fall points floor' sand' =
    let
        d =
            (move D sand')

        dl =
            (move DL sand')

        dr =
            (move DR sand')

        pointsCheck =
            case floor' of
                (Infinite y) ->
                    (Point (_x d) y) : (Point (_x dl) y) : (Point (_x dr) y) : points

                (Platform _ _) ->
                    points
    in
    case ( d `elem` pointsCheck, dl `elem` pointsCheck, dr `elem` pointsCheck ) of
        ( False, _, _ ) ->
            continue d

        ( _, False, _ ) ->
            continue dl

        ( _, _, False ) ->
            continue dr

        _ ->
            sand'
    where
        continue x =
            if finish floor' x then x else fall points floor' x


isSand :: Material -> Bool
isSand (Rock _) =
    False
isSand (Sand _) =
    True


point :: Material -> Point
point (Rock p) =
    p
point (Sand p) =
    p


move :: Direction -> Point -> Point
move D (Point x y) =
    Point x (y + 1)
move DL (Point x y) =
    Point (subtract 1 x) (y + 1)
move DR (Point x y) =
    Point (x + 1) (y + 1)


sand :: Point
sand =
    Point 500 0


interpolate :: Path -> Path
interpolate (a@(Point ax ay) : b@(Point bx by) : xs)
    | ax == bx && abs (ay - by) > 1 =
        (++ (interpolate (b : xs))) . (a :) . map (\y -> Point ax y) $ range ay by
    | ay == by && abs (ax - bx) > 1 =
        (++ (interpolate (b : xs))) . (a :) . map (\x -> Point x ay) $ range ax bx
    | otherwise =
        a : interpolate (b : xs)
    where
        range a' b' =
            [((+ 1) $ min a' b')..(subtract 1 $ max a' b')]
interpolate path =
    path


parse :: String -> Cave
parse s =
    case readP_to_S parseFile s of
        (( p, _ ) : _) ->
            let
                points =
                    sort . nub . concat . map interpolate $ p

                bl =
                    head points

                br =
                    (\x -> Point x (_y bl)) . head . reverse . sort . map _x $ points
            in
            Cave bl br (map Rock points)

        _ ->
            Cave (Point 0 0) (Point 0 0) []


parseFile :: ReadP [Path]
parseFile =
    endBy parsePath (char '\n') <* eof


parsePath :: ReadP Path
parsePath =
    sepBy parsePoint (string " -> ")


parsePoint :: ReadP Point
parsePoint = do
    x <- number
    _ <- char ','
    y <- number
    return $ Point x y


number :: ReadP Int
number =
    (fmap read . munch1) isDigit
