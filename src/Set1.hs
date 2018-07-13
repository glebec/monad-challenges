{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

-- 1.1
-- rand :: Seed -> (Integer, Seed)
-- mkSeed :: Integer -> Seed

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

rands :: [(Integer, Seed)]
rands = (0, mkSeed 1) : map (rand . snd) rands

fiveRands :: [Integer]
fiveRands = map fst $ take 5 $ tail' rands

-- 1.2
-- toLetter :: Integer -> Char

randLetter :: Gen Char
randLetter s = let (i, s') = rand s in (toLetter i, s')

randString3 :: String
randString3 = take 3 $ map (fst . randLetter . snd) rands

-- 1.3

type Gen a = Seed -> (a, Seed)

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (* 2)

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA ((+ 1) . (* 2))

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (* 10)

generalA :: (Integer -> Integer) -> Gen Integer
generalA f s = let (n, s') = rand s in (f n, s')
