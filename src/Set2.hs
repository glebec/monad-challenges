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

-- 2.4

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain f Nothing  = Nothing
chain f (Just a) = f a

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 g s = lookupMay s g
    `link` (\xs ->
        tailMay xs
        `link` maximumMay
        `link` (\m ->
            headMay xs
            `link` \h -> fromIntegral m `divMay` fromIntegral h))

-- 2.5
-- salaries :: [(String, Integer)]

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries ss n1 n2 = case (lookupMay n1 ss, lookupMay n2 ss) of
    (Nothing, _      ) -> Nothing
    (_,       Nothing) -> Nothing
    (Just s1, Just s2) -> mkMaybe $ s1 + s2

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f ma mb =
    ma `link` (\a ->
        mb `link` (\b ->
            mkMaybe $ f a b))
-- yLink f ma mb = case (ma, mb) of
--     (Nothing, _     ) -> Nothing
--     (_,      Nothing) -> Nothing
--     (Just a, Just b ) -> Just $ f a b

addSalaries2 :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries2 ss n1 n2 = yLink (+) (lookupMay n1 ss) (lookupMay n2 ss)

mkMaybe :: a -> Maybe a
mkMaybe = Just
