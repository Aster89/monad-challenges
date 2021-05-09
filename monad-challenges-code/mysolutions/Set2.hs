{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set2 where

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " ++ show x


instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = False
  Just x == Just y = x == y
  _ == _ = False


headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (a:_) = Just a

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:a) = Just a

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay a ((k,v):kv)
  | a == k = Just v
  | otherwise = lookupMay a kv

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just $ x / y

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay (a:as) = Just $ max' a (maximumMay as)
  where
    max' x Nothing = x
    max' x (Just y) = max x y

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay (a:as) = Just $ min' a (minimumMay as)
  where
    min' x Nothing = x
    min' x (Just y) = min x y

queryGreek :: GreekData -> String -> Maybe Double
queryGreek g s = let xs = lookupMay s g
                 in case xs of
                   Nothing -> Nothing
                   (Just xs') -> let t = tailMay xs'
                                 in case t of
                                   Nothing -> Nothing
                                   (Just t') -> let m = maximumMay t'
                                                in case m of
                                                  Nothing -> Nothing
                                                  (Just m') -> let h = headMay xs'
                                                               in case h of
                                                                 Nothing -> Nothing
                                                                 (Just h') -> fromInteger m' `divMay` fromInteger h'

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link x f = case x of
  Nothing -> Nothing
  (Just y) -> f y

queryGreek2 :: GreekData -> String -> Maybe Double
queryGreek2 g s = let v = Just g `link` lookupMay s
                      h = v `link` headMay `link` (Just . fromInteger)
                      m = v `link` tailMay `link` maximumMay `link` (Just . fromInteger)
                  in m `link` \y -> h `link` \x -> y `divMay` x


addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries l n1 n2 = let s1 = lookupMay n1 l
                          s2 = lookupMay n2 l
                      in s1 `link` \s1' -> s2 `link` \s2' -> Just (s1' + s2')

{- knowing some functions already doesn't let me think differently :/ -}
yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f x y = x `link` \x' -> y `link` \y' -> Just (x' `f` y')

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on f g x y = f (g x) (g y)

addSalaries' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries' l = yLink (+) `on` (`lookupMay` l)

tailProd :: Num a => [a] -> Maybe a
tailProd l = tailMay l `link` (Just . product)

tailSum :: Num a => [a] -> Maybe a
tailSum l = tailMay l `link` (Just . sum)

{- again, I know this stuff already, it is `fmap`! Having read LYAH pays off! -}
transMaybe :: (a -> b) -> Maybe a -> Maybe b
transMaybe f = (`link` (Just . f))

tailProd' :: Num a => [a] -> Maybe a
tailProd' l = transMaybe product $ tailMay l
tailSum' :: Num a => [a] -> Maybe a
tailSum' l = transMaybe sum $ tailMay l

{- fot these two I had used `link` instead of `transMaybe`, which gave me the result already flattened -}
tailMax :: Ord a => [a] -> Maybe (Maybe a)
tailMax l = transMaybe maximumMay $ tailMay l
tailMin :: Ord a => [a] -> Maybe (Maybe a)
tailMin l = transMaybe minimumMay $ tailMay l

combine {- this is `join` -} :: Maybe (Maybe a) -> Maybe a
combine = (`link` id)
