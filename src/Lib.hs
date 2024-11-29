module Lib
( isSorted, multiplyMod) where
import Data.List


multiplyMod::Int->Int->Int-> Int
multiplyMod a b m =  mod (mod a m *mod b m) m

{-
isSorted:: Ord a => [a] ->Bool -> Bool
isSorted list direction | direction = list == sort list
                        | otherwise = list  == reverse (sort list)-}
isSorted :: Ord a => [a] -> Bool -> Bool
isSorted list direction 
  | direction = list == sort list
  | otherwise = list == reverse (sort list)