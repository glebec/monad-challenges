{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set5 where

import MCPrelude
import Set4 (Monad(..), (>>=))

-- 5.1

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = do
    a  <- m
    as <- sequence' ms
    return $ a : as
