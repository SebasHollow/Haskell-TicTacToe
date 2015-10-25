module Board where

import Data.List
import Data.Maybe

getEmptyBoard :: [(Int, Int, Char)]
getEmptyBoard = [(x, y, ' ') | x <- [0..2], y <- [0..2]]

replaceValue :: (Int, Int, Char) -> Char -> (Int, Int, Char)
replaceValue (x, y, _) c = (x, y, c)

getSquareValue :: (Int, Int, Char) -> Char
getSquareValue (_, _, c) = c

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n item list = a ++ (item:b)
    where (a, (_:b)) = splitAt n list

replaceTuple :: [(Int, Int, Char)] -> (Int, Int, Char) -> [(Int, Int, Char)]
replaceTuple list tuple  = replaceAt i tuple list
    where (x, y, _) = tuple
          i = 3 * x + y

getEmptyIndex :: [(Int, Int, Char)] -> Maybe Int
getEmptyIndex list
    | null list = Nothing
    | getSquareValue (head list) == ' ' = Just (0)
    | otherwise = if (i /= Nothing)
        then fmap (\a -> a + 1) $ i
        else Nothing
    where i = (getEmptyIndex (tail list))

getNewBoard :: [(Int, Int, Char)] -> Maybe (Int, Int, Char)
getNewBoard positions = answer
    where i = getEmptyIndex board
          board = loadBoard getEmptyBoard positions
          char = getTurn board
          ans = replaceValue (board !! fromJust i) char
          answer = if (i /= Nothing) then Just ans else Nothing

getTurn :: [(Int, Int, Char)] -> Char
getTurn board = if (o < x) then 'o' else 'x'
    where x = count 'x'
          o = count 'o'
          count = (\c -> sum [1 | (_, _, v) <- board, v == c])

loadBoard :: [(Int, Int, Char)] -> [(Int, Int, Char)] -> [(Int, Int, Char)]
loadBoard board items
    | len == 0 = board
    | len == 1 = newBoard
    | otherwise = loadBoard (newBoard) hs
    where len = length items
          (h:hs) = items
          newBoard = replaceTuple board h