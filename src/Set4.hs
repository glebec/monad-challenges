{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2 (Maybe(..), link)

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

randLetter :: Gen Char
-- randLetter = Gen rand >>= (return . toLetter)
randLetter = toLetter <$> Gen rand

randString3 :: String
-- randString3 = evalGen (sequence $ replicate 3 randLetter) seed1
randString3 = evalGen (replicateM 3 randLetter) seed1

-- 4.6.1.3

randEven :: Gen Integer
randEven = (*2) <$> Gen rand

randOdd :: Gen Integer
randOdd = (+1) <$> randEven

randTen :: Gen Integer
randTen = (*10) <$> Gen rand
