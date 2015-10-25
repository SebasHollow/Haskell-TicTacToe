--Tic Tac Toe message parser
module Parser where

import Data.List.Split


-- parse all Members
parse :: String -> [(Int, Int, Char)]
parse str = m
    where members = parseMems (delL str)
          m = map parseM members

delL :: String -> String
delL ('l':_:xs) = init xs
delL a = a

parseMems :: String -> [String]
parseMems str
    | str == [] = []
    | otherwise = x
    where (_:x) = m
          m = splitOn "m" str

-- parse a Member
parseM :: String -> (Int, Int, Char)
parseM string = toTuple (x:y:v:[])
    where elems = splitOn " " string
          (x:_) = get (elems !! 1)
          (y:_) = get (elems !! 3)
          v     = parseValue $ elems !! 5
          get   = splitOn ";"

parseValue :: String -> String
parseValue str =
    let
        strArray = splitOn "\"" str
    in
        strArray !! 1

toTuple :: [String] -> (Int, Int, Char)
toTuple string = (x, y, c)
    where x = read (string !! 0) :: Int
          y = read (string !! 1) :: Int
          c = head $ last string
