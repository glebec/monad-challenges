{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set1 (Gen, generalA, mkGen, genTwo)

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
