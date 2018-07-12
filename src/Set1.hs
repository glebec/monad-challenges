{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

fiveRands :: [Integer]
fiveRands = map fst $ take 5 $ tail' rands
  where rands = (0, mkSeed 1) : map (rand . snd) rands
