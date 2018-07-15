{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude

-- 3.1

allPairs :: [a] -> [b] -> [(a,b)]
-- allPairs xs ys = [(x, y) | x <- xs, y <- ys]
allPairs [] _  = []
allPairs _  [] = []
allPairs (x:xs) l2 = map ((,) x) l2 ++ allPairs xs l2

-- 3.2

data Card = Card Int String deriving (Eq)

instance Show Card where
    show (Card rank suit) = show rank ++ suit

allCards :: [Int] -> [String] -> [Card]
allCards [] _ = []
allCards _ [] = []
allCards (r:rs) suits = map (Card r) suits ++ allCards rs suits
