module AdventOfCode.Day13
    ( run
    ) where

import Data.Char (isDigit)
import Data.List (sort)
import Text.ParserCombinators.ReadP


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . sum . filterIndexes (== LT) . map (uncurry compare) . parse

        solve2 =
            show . product . filterIndexes isDivider . sort . (++ dividers) . flatten . parse

        flatten =
            concat . map (\( a, b ) -> a : [ b ])

        filterIndexes f =
            map fst . filter (f . snd) . zip [(1 :: Int)..]


type Pair = ( Packet, Packet )


data Packet
    = Byte Int
    | List [Packet]
    deriving Eq


instance Show Packet where
    show (Byte b) =
        show b
    show (List l) =
        show l


instance Ord Packet where
    compare (Byte a) (Byte b) =
        compare a b
    compare (List a) (List b) =
        compare a b
    compare a@(List _) b@(Byte _) =
        compare a (List [ b ])
    compare a@(Byte _) b@(List _) =
        compare (List [ a ]) b


isDivider :: Packet -> Bool
isDivider p =
    p `elem` dividers


dividers :: [Packet]
dividers =
    [ List [ List [ Byte 2 ] ], List [ List [ Byte 6 ] ] ]


parse :: String -> [Pair]
parse s =
    case readP_to_S parseFile s of
        (( pairs, _ ) : _) ->
            pairs

        _ ->
            []


parseFile :: ReadP [Pair]
parseFile =
    sepBy parsePair (char '\n') <* eof


parsePair :: ReadP Pair
parsePair = do
    top <- terminated parsePacket
    bottom <- terminated parsePacket
    return $ ( top, bottom )


parsePacket :: ReadP Packet
parsePacket =
    fmap (List . foldl (++) []) $
        between (char '[') (char ']') $ sepBy numberOrPacket (char ',')
    where
        numberOrPacket =
            (fmap ((: []) . Byte) number)
                <++ (fmap (: []) parsePacket)


number :: ReadP Int
number =
    (fmap read . munch1) isDigit


terminated :: ReadP a -> ReadP a
terminated =
    (<* (char '\n'))
