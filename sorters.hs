module Sorters where

import Data.List

selectionSort :: Eq a => Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = minimum xs
                   in x : selectionSort (delete x xs)

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) = let less = filter (<= x) xs
                       more = filter (> x) xs
                   in quickSort less ++ [x] ++ quickSort more