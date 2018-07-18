{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude
import Set4 ( Monad(..)
            , (>>=)
            , Gen(..)
            , randLetter)

-- 5.1

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = do
    a  <- m
    as <- sequence' ms
    return $ a : as

-- 5.2 -- just modified `Monad` in §4 instead

-- class Monad m where
--     (>>=)  :: m a -> (a -> m b) -> m b
--     return :: a -> m a
--     fail   :: String -> m a
--     fail = undefined

-- 5.3

makeRandom :: Gen Integer
makeRandom = Gen rand

fiveRands :: Gen [Integer]
-- fiveRands = evalGen (replicateM 5 (Gen rand)) seed1
fiveRands = do
    r1 <- makeRandom
    r2 <- makeRandom
    r3 <- makeRandom
    r4 <- makeRandom
    r5 <- makeRandom
    return [r1, r2, r3, r4, r5]

randString :: Gen String
randString = do
    c1 <- randLetter
    c2 <- randLetter
    c3 <- randLetter
    return [c1, c2, c3]

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair ga gb = do
    a <- ga
    b <- gb
    return (a, b)
