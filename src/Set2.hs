{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

-- 2.1

data Maybe a = Nothing | Just a deriving (Eq) -- normally would derive Show

-- added as part of the exercise
instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just a) = "Just " ++ show a

-- 2.2

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:xs) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (x:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a ((a', b):xs) = if   a == a'
                           then Just b
                           else lookupMay a xs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay a b = Just $ a / b

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just $ foldr1 max xs

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just $ foldr1 min xs

-- 2.3
-- type GreekData = [(String, [Integer])]

queryGreek :: GreekData -> String -> Maybe Double
queryGreek g s = case lookupMay s g of
    Nothing   -> Nothing
    (Just xs) -> case tailMay xs of
        Nothing  -> Nothing
        (Just t) -> case maximumMay t of
            Nothing  -> Nothing
            (Just m) -> case headMay xs of
                Nothing  -> Nothing
                (Just h) -> fromIntegral m `divMay` fromIntegral h
