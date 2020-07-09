module Lib
 ( 
  someFunc,
  qsort
 ) where


qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
 where lhs = filter  (< x) xs
       rhs = filter (>= x) xs

someFunc :: IO ()
someFunc = putStrLn "someFunc"
