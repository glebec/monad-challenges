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

-- 3.3

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs _ [] _ = []
allCombs _ _ [] = []
allCombs f (x:xs) l2 = map (f x) l2 ++ allCombs f xs l2

allPairs2 :: [a] -> [b] -> [(a,b)]
allPairs2 = allCombs (,)

allCards2 :: [Int] -> [String] -> [Card]
allCards2 = allCombs Card

-- 3.4

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 _ [] _ _ = []
allCombs3 _ _ [] _ = []
allCombs3 _ _ _ [] = []
allCombs3 f lx@(x:xs) ly lz = let fxys = map (f x) ly in
    allCombs ($) fxys lz ++ allCombs3 f xs ly lz

-- 3.5

combStep :: [a -> b] -> [a] -> [b]
combStep [] _ = []
combStep _ [] = []
combStep (f:fs) as = map f as ++ combStep fs as

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' f = combStep . combStep [f]

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f as bs = combStep (combStep (combStep [f] as) bs)
