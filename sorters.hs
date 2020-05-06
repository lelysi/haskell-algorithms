module Sorters where

import Data.List

selectionSort :: Eq a => Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = minimum xs
                   in x : selectionSort (delete x xs)