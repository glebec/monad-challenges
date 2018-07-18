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

newtype Gen a = Gen (Seed -> (a, Seed))

instance Monad Gen where
    return a = Gen (\s -> (a, s))
    bind (Gen fa) fgb = Gen (\s ->
        let (a, s') = fa s in
        let (Gen fb) = fgb a in
        fb s')

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
