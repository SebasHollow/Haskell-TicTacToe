--Tic Tac Toe message parser
module Parser where

import Data.List.Split


-- parse all Members
parse :: String -> [(Int, Int, Char)]
parse str = map parseValue $ parseElems str

parseElems :: String -> [String]
parseElems str
    | str == [] = []
    | otherwise = x
    where (_:x) = m
          m = splitOn "m" str

-- parse a Member
parseValue :: String -> (Int, Int, Char)
parseValue string = toTuple (x:y:v:[])
    where elems = splitOn "; " string
          x = (elems !! 1)
          y = (elems !! 3)
          (_:v:_) = splitOn "\"" $ elems !! 5

toTuple :: [String] -> (Int, Int, Char)
toTuple string = (x, y, c)
    where x = read (string !! 0) :: Int
          y = read (string !! 1) :: Int
          c = head $ last string
