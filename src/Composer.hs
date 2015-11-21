module Composer where

import Data.List
import Text.Printf

serialize :: [(Int, Int, Char)] -> String
serialize squares = addToList str
    where str = intercalate "; " sqStrings
          sqStrings = map squareToString squares


squareToString :: (Int, Int, Char) -> String
squareToString (x, y, c) = printf format x y c
    where format = "m[\"x\"; %d; \"y\"; %d; \"v\"; \"%c\"]"

addToList :: String -> String
addToList mems = printf format mems
    where format = "l[%s]"
