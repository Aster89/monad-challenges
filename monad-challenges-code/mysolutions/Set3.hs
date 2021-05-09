{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set3 where

import MCPrelude


allPairs :: [a] -> [b] -> [(a,b)]
allPairs = (concat .) . allPairs'
  where
    allPairs' [] _ = []
    allPairs' (a:as) b = allPairs'' a b:allPairs' as b
    allPairs'' _ [] = []
    allPairs'' a (b:bs) = (a,b):allPairs'' a bs

data Card = Card Int String

instance Show Card where
  show (Card i s) = show i ++ s

allCards :: [Int] -> [String] -> [Card]
allCards = (concat .) . allCards'
  where
    allCards' [] _ = []
    allCards' (a:as) b = allCards'' a b:allCards' as b
    allCards'' _ [] = []
    allCards'' a (b:bs) = Card a b:allCards'' a bs

allCombs {- This is essentially `liftM2`/`liftA2` for lists -} :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs f = (concat .) . allCombs'
  where
    allCombs' [] _ = []
    allCombs' (a:as) b = allCombs'' a b:allCombs' as b
    allCombs'' _ [] = []
    allCombs'' a (b:bs) = f a b:allCombs'' a bs

allPairs' :: [a] -> [b] -> [(a,b)]
allPairs' = allCombs (,)
allCards' :: [Int] -> [String] -> [Card]
allCards' = allCombs Card

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f x y = (concat . concat) . allCombs3' x y
  where
    allCombs3' [] _ _ = []
    allCombs3' (a:as) b c = allCombs3'' a b c:allCombs3' as b c
    allCombs3'' _ [] _ = []
    allCombs3'' a (b:bs) c = allCombs3''' a b c:allCombs3'' a bs c
    allCombs3''' _ _ [] = []
    allCombs3''' a b (c:cs) = f a b c:allCombs3''' a b cs

{- Knowing applicatives from LYAH, this was an easy one! -}
pure' :: a -> [a]
pure' x = [x]
combStep :: [a -> b] -> [a] -> [b]
combStep = (concat .) . helper
  where
    helper [] _ = []
    helper (f:fs) xs = helper' f xs:helper fs xs
    helper' _ [] = []
    helper' f (x:xs) = f x:helper' f xs

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' f x y z = pure' f `combStep` x `combStep` y `combStep` z
