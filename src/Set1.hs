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
randLetter = generalA toLetter rand

randString3 :: String
randString3 = take 3 $ map (fst . randLetter . snd) rands

-- 1.3

type Gen a = Seed -> (a, Seed)

randEven :: Gen Integer -- the output of rand * 2
randEven = generalA (* 2) rand

randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd = generalA ((+ 1) . (* 2)) rand

randTen :: Gen Integer -- the output of rand * 10
randTen = generalA (* 10) rand

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g s = let (  a, s') = g s
                 in  (f a, s')

-- 1.4

randPair :: Gen (Char, Integer)
randPair s = ((c, i), s'')
    where (c, s')  = randLetter s
          (i, s'') = rand s'

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb s = ((a, b), s'')
    where (a, s')  = ga s
          (b, s'') = gb s'

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f ga gb s = (f a b, s'')
    where (a, s')  = ga s
          (b, s'') = gb s'

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

-- 1.5

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([], s)
repRandom l  s = (map fst ps, snd $ last' ps)
  where ps = go l s
        go []     s' = []
        go (g:gs) s' = let p@(a, s'') = g s'
                       in  p : go gs s''

-- 1.6

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo ga f s = let (a, s') = ga s
                in  f a s'

mkGen :: a -> Gen a
mkGen a s = (a, s)

-- 4.2

generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f ga gb = ga `genTwo` (\a -> gb `genTwo` (mkGen . f a))

-- mkGen :: a -> Gen a
-- genTwo :: Gen a -> (a -> Gen b) -> Gen b
-- generalA :: (a -> b) -> Gen a -> Gen b
repRandom2 :: [Gen a] -> Gen [a]
repRandom2 [] = mkGen []
repRandom2 (g:gs) =
    g `genTwo` (\a ->
        repRandom2 gs `genTwo` (\as ->
            mkGen $ a : as))
-- well THAT was mind-bending, and I didn't use `generalA`, so now I'm worried.
