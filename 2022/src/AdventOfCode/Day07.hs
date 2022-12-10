module AdventOfCode.Day07
    ( run
    ) where

import Data.List (isSuffixOf, foldl', minimumBy)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map


run :: String -> ( String, String )
run input =
    ( solve1 input, solve2 input )
    where
        solve1 =
            show . sum . filter (<= 100000) . map snd . allDirSizes . filesystem

        solve2 =
            show . snd . minimumBy minDirSize . overSpaceNeeded . filesystem

        filesystem =
            buildFilesystem . parseCommands . lines

        rootSize =
            dirSize [ "/" ]

        unusedSpaced =
            (70000000 -) . rootSize

        spacedNeeded =
            ((-) 30000000) . unusedSpaced

        overSpaceNeeded fs =
            filter ((>= (spacedNeeded fs)) . snd) . allDirSizes $ fs

        minDirSize a b =
            compare (snd a) (snd b)


type Filesystem = Map.Map Path Contents


type Path = [String]


type Contents = [Entry]


data Entry
    = Directory { _name :: String }
    | File { _name :: String, _size :: Int }
    deriving Show


data Command
    = Cd { _dir :: String }
    | Ls { _entries :: [Entry] }
    deriving Show


allDirSizes :: Filesystem -> [( Path, Int )]
allDirSizes fs =
    map (\path -> ( path, dirSize path fs )) . Map.keys $ fs


dirSize :: Path -> Filesystem -> Int
dirSize path =
    Map.foldrWithKey f 0
    where
        f key dir size =
            (+ size) (if (path `isSuffixOf` key) then sum $ map entrySize dir else 0)


entrySize :: Entry -> Int
entrySize (Directory _) =
    0
entrySize (File _ size) =
    size


buildFilesystem :: [Command] -> Filesystem
buildFilesystem =
    snd . foldl' processCommand ( [], Map.empty )


processCommand :: ( Path, Filesystem ) -> Command -> ( Path, Filesystem )
processCommand ( cwd, fs ) (Cd "..") =
    ( tail cwd, fs )
processCommand ( cwd, fs ) (Cd dir) =
    ( dir : cwd, fs )
processCommand ( cwd, fs ) (Ls entries) =
    ( cwd, Map.insert cwd entries fs )


parseCommands :: [String] -> [Command]
parseCommands =
    catMaybes . parseCommands'


parseCommands' :: [String] -> [Maybe Command]
parseCommands' (('$' : ' ' : command) : xs) =
    (\( command', xs' ) -> command' : parseCommands' xs') $ parseCommand command xs
parseCommands' _ =
    []


parseCommand :: String -> [String] -> ( Maybe Command, [String] )
parseCommand ('c' : 'd' : ' ' : dir) xs =
    ( Just $ Cd dir, xs )
parseCommand ('l' : 's' : _) xs =
    (\( entries, xs' ) -> ( Just $ Ls entries, xs' )) $ parseEntries xs
parseCommand _ xs =
    ( Nothing, xs )


parseEntries :: [String] -> ( [Entry], [String] )
parseEntries xs@(('$' : _) : _) =
    ( [], xs )
parseEntries (entry : xs) =
    case (parseEntry entry) of
        Just entry' ->
            (\( entries, xs' ) -> ( entry' : entries, xs' )) $ parseEntries xs

        Nothing ->
            parseEntries xs
parseEntries xs =
    ( [], xs )


parseEntry :: String -> Maybe Entry
parseEntry s =
    case (break (== ' ') s) of
        ( ('d' : 'i' : 'r' : []), (_ : name) ) ->
            Just $ Directory name

        ( size, (_ : name) ) ->
            Just $ File name (read size)

        _ ->
            Nothing
