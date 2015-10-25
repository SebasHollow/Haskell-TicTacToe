module Board where

import Data.List
import Data.Maybe

emptyBoard :: [(Int, Int, Char)]
emptyBoard = [(x, y, ' ') | x <- [0..2], y <- [0..2]]

-- Replace functions
replaceChar :: (Int, Int, Char) -> Char -> (Int, Int, Char)
replaceChar (x, y, _) c = (x, y, c)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n item list = a ++ (item:b)
    where (a, (_:b)) = splitAt n list

replaceTuple :: [(Int, Int, Char)] -> (Int, Int, Char) -> [(Int, Int, Char)]
replaceTuple list tuple  = replaceAt i tuple list
    where (x, y, _) = tuple
          i = 3 * x + y


getNextTurn :: [(Int, Int, Char)] -> Maybe (Int, Int, Char)
getNextTurn positions = nextTurn
    where board = loadBoard emptyBoard positions
          i = getEmptyIndex board
          v = board !! fromJust i
          nextTurn = if (i /= Nothing)
                        then Just $ replaceChar v (getTurn board)
                        else Nothing

loadBoard :: [(Int, Int, Char)] -> [(Int, Int, Char)] -> [(Int, Int, Char)]
loadBoard board items
    | len == 0 = board
    | len == 1 = newBoard
    | otherwise = loadBoard (newBoard) hs
    where len = length items
          (h:hs) = items
          newBoard = replaceTuple board h

getEmptyIndex :: [(Int, Int, Char)] -> Maybe Int
getEmptyIndex list
    | null list = Nothing
    | c == ' ' = Just (0)
    | otherwise = fmap (\a -> a + 1) $ i
    where i = (getEmptyIndex (tail list))
          (_, _, c) = (head list)


getTurn :: [(Int, Int, Char)] -> Char
getTurn board = if (o < x) then 'o' else 'x'
    where x = count 'x'
          o = count 'o'
          count = (\c -> sum [1 | (_, _, v) <- board, v == c])