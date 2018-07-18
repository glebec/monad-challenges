{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude
import Set2 ( Maybe(..)
            , link
            , headMay
            , tailMay
            , lookupMay
            , divMay
            , maximumMay
            , minimumMay)
import Set3 ( Card(..))
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

-- 5.4

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gs s = do
    xs <- lookupMay s gs
    t  <- tailMay xs
    m  <- maximumMay t
    h  <- headMay xs
    fromIntegral m `divMay` fromIntegral h

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ts n1 n2 = do
    s1 <- lookupMay n1 ts
    s2 <- lookupMay n2 ts
    return $ s1 + s2

tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
    t <- tailMay xs
    return $ product t

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
    t <- tailMay xs
    return $ sum t

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do
    t <- tailMay xs
    maximumMay t

-- 5.5

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = do
    x <- xs
    y <- ys
    return (x, y)

allCards :: [Int] -> [String] -> [Card]
allCards rs ss = do
    r <- rs
    s <- ss
    return $ Card r s

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = do
    x <- xs
    y <- ys
    z <- zs
    return $ f x y z
