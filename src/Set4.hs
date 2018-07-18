{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2 ( Maybe(..)
            , link
            , headMay
            , tailMay
            , lookupMay
            , divMay
            , maximumMay
            , minimumMay)
import Set3 (Card(..))

-- 4.1

-- generalPair :: Gen a -> Gen b -> Gen (a, b)
-- repRandom :: [Gen a] -> Gen [a]
-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
-- transMaybe :: (a -> b) -> Maybe a -> Maybe b
-- combine :: Maybe (Maybe a) -> Maybe a

-- mkM     :: a -> m     a
-- mkGen   :: a -> Gen   a
-- mkMaybe :: a -> Maybe a

-- linkM  :: m     a -> (a -> m     b) -> m     b
-- genTwo :: Gen   a -> (a -> Gen   b) -> Gen   b
-- link   :: Maybe a -> (a -> Maybe b) -> Maybe b

-- yLinkM   :: (a -> b -> c) -> m     a -> m     b -> m     c
-- generalB :: (a -> b -> c) -> Gen   a -> Gen   b -> Gen   c
-- yLink    :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

-- 4.2

-- moved to Set1 because of `Gen` newtype incompatibility

-- 4.3

class Monad m where
    bind :: m a -> (a -> m b) -> m b
    return :: a -> m a

linkM :: Monad m => (a -> b -> c) -> m a -> m b -> m c
linkM f ma mb = ma `bind` (\a -> mb `bind` (return . f a))

-- 4.4

newtype Gen a = Gen { runGen :: Seed -> (a, Seed) }

instance Monad Gen where
    return a = Gen (\s -> (a, s))
    bind ga fgb = Gen (\s ->
        let (a, s') = runGen ga s in runGen (fgb a) s')

instance Monad Maybe where
    return = Just
    bind = link

instance Monad [] where
    return = (:[])
    bind lx fx2l = case lx of
        [] -> []
        (x:xs) -> fx2l x ++ bind xs fx2l

evalGen :: Gen a -> Seed -> a
evalGen (Gen f) s = let (a, s') = f s in a

-- 4.5

-- 4.5.1

-- repRandom :: [Gen a] -> Gen [a]
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (m:ms) =
    m `bind` (\a ->
        sequence ms `bind` (\as ->
            return $ a : as))

-- generalB :: (a -> b -> c) -> Gen   a -> Gen   b -> Gen   c
-- yLink    :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
-- allCombs :: (a -> b -> c) -> [a]     -> [b]     -> [c]
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = ma `bind` (\a -> mb `bind` (return . f a))

-- 4.5.2

-- chain :: (a -> Maybe b) -> Maybe a -> Maybe b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

-- combine :: Maybe (Maybe a) -> Maybe a
join :: Monad m => m (m a) -> m a
join = (`bind` id)

-- Not part of the exercises, but the above gave me the epiphany that monads
-- can change type arguments, and are therefore a superclass of functors!
fmapM :: Monad m => (a -> b) -> m a -> m b
fmapM f ma = ma `bind` (return . f)

-- actually this ends up being useful later, so here's an operator for it
(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = fmapM

-- 4.5.3

-- adding this for convenience
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) = bind

-- allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f ma mb mc =
    ma >>= (\a ->
    mb >>= (\b ->
    mc >>= (\c ->
    return $ f a b c)))

-- combStep :: [a -> b] -> [a] -> [b]
ap :: Monad m => m (a -> b) -> m a -> m b
ap mf ma =
    mf >>= (\f ->
    ma >>= (\a ->
    return $ f a))

-- 4.6

-- 4.6.1

-- 4.6.1.1

seed1 :: Seed
seed1 = mkSeed 1

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n ma = sequence $ replicate n ma

fiveRands :: [Integer]
-- fiveRands = evalGen (sequence $ replicate 5 (Gen rand)) seed1
fiveRands = evalGen (replicateM 5 (Gen rand)) seed1

-- 4.6.1.2

randInteger :: Gen Integer
randInteger = Gen rand

randLetter :: Gen Char
-- randLetter = Gen rand >>= (return . toLetter)
randLetter = toLetter <$> randInteger

randString3 :: String
-- randString3 = evalGen (sequence $ replicate 3 randLetter) seed1
randString3 = evalGen (replicateM 3 randLetter) seed1

-- 4.6.1.3

randEven :: Gen Integer
randEven = (*2) <$> randInteger

randOdd :: Gen Integer
randOdd = (+1) <$> randEven

randTen :: Gen Integer
randTen = (*10) <$> randInteger

-- 4.6.1.4

randPair :: Gen (Char, Integer)
randPair = liftM2 (,) randLetter randInteger

-- generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair :: Monad m => m a -> m b -> m (a,b)
generalPair = liftM2 (,)

-- 4.6.1.5

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

-- 4.6.1.6

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo = bind

mkGen :: a -> Gen a
mkGen = return

-- 4.6.2

-- 4.6.2.1 (imported)

-- 4.6.2.2 (imported)

-- headMay :: [a] -> Maybe a
-- tailMay :: [a] -> Maybe [a]
-- lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
-- divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
-- maximumMay :: Ord a => [a] -> Maybe a
-- minimumMay :: Ord a => [a] -> Maybe a

-- 4.6.2.3

queryGreek :: GreekData -> String -> Maybe Double
queryGreek gs s =
    lookupMay s gs >>= \xs ->
    tailMay xs     >>= \t  ->
    maximumMay t   >>= \m  ->
    headMay xs     >>= \h  ->
    fromIntegral m `divMay` fromIntegral h

-- 4.6.2.4

-- `chain` as (=<<) already implemented in 4.5.2
-- `link` as (>>=) already implemented in 4.3–4.4

-- 4.6.2.5

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ts n1 n2 = linkM (+) (lookupMay n1 ts) (lookupMay n2 ts)

mkMaybe :: a -> Maybe a
mkMaybe = return

-- 4.6.2.6

transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe = fmapM

tailProd :: Num a => [a] -> Maybe a
tailProd = fmapM product . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = fmapM sum . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax = join . fmapM maximumMay . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = join . fmapM minimumMay . tailMay

-- 4.6.3

-- 4.6.3.1

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = liftM2 (,)

-- 4.6.3.2

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card

-- 4.6.3.3

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs = liftM2

-- 4.6.3.4

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 = liftM3
