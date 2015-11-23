module Board where

import Data.List
import Data.Maybe

type Square = (Int, Int, Char)

emptyBoard :: [Square]
emptyBoard = [(x, y, ' ') | x <- [0..2], y <- [0..2]]

-- Replace functions
replaceChar :: Square -> Char -> Square
replaceChar (x, y, _) c = (x, y, c)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt n item list = a ++ (item:b)
    where (a, (_:b)) = splitAt n list

replaceTuple :: [Square] -> Square -> [Square]
replaceTuple list tuple  = replaceAt i tuple list
    where (x, y, _) = tuple
          i = 3 * x + y


getNextTurn :: [Square] -> Maybe Square
getNextTurn positions = nextTurn
    where board = loadBoard emptyBoard positions
          i = getEmptyIndex board
          v = board !! fromJust i
          nextTurn = if (i /= Nothing)
                        then Just $ replaceChar v (getTurn board)
                        else Nothing

loadBoard :: [Square] -> [Square] -> [Square]
loadBoard board items
    | len == 0 = board
    | len == 1 = newBoard
    | otherwise = loadBoard (newBoard) hs
    where len = length items
          (h:hs) = items
          newBoard = replaceTuple board h

getEmptyIndex :: [Square] -> Maybe Int
getEmptyIndex list
    | null list = Nothing
    | c == ' ' = Just (0)
    | otherwise = fmap (\a -> a + 1) $ i
    where i = (getEmptyIndex (tail list))
          (_, _, c) = (head list)


getTurn :: [Square] -> Char
getTurn board = if (o < x) then 'o' else 'x'
    where x = count 'x'
          o = count 'o'
          count = (\c -> sum [1 | (_, _, v) <- board, v == c])