module Fibonacci where

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

fibonacci' 0 = 0
fibonacci' n = fibHelp 1 0 n where
	fibHelp x y 1 = x + y
	fibHelp x y n = fibHelp y (x + y) (n - 1)