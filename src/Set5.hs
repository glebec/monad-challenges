{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude
-- import Set4 (Monad(..), (>>=)) -- replaced in 5.2

-- 5.1

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = do
    a  <- m
    as <- sequence' ms
    return $ a : as

-- 5.2

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a
    fail   :: String -> m a
    fail = undefined
