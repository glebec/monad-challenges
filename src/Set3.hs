{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
-- allPairs xs ys = [(x, y) | x <- xs, y <- ys]
allPairs [] _  = []
allPairs _  [] = []
allPairs (x:xs) l2 = map ((,) x) l2 ++ allPairs xs l2
