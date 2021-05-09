{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1a where

import MCPrelude

import Set4 (Gen(..), evalGen)
import Set2 (Maybe(..), lookupMay, headMay, tailMay, maximumMay, divMay)
import Set3 (Card(..))

class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

    fail :: String -> m a
    fail = undefined

instance Monad Gen where
  return a = Gen $ \s -> (a, s)
  g >>= f = Gen $ \s -> let (a,s') = run g s
                        in run (f a) s'

makeRandom :: Gen Integer
makeRandom = Gen rand

fiveRands :: Gen [Integer]
fiveRands = do
  r0 <- makeRandom
  r1 <- makeRandom
  r2 <- makeRandom
  r3 <- makeRandom
  r4 <- makeRandom
  return [r0,r1,r2,r3,r4]

randLetter :: Gen Char
randLetter = do
  r <- makeRandom
  return $ toLetter r

randString3 :: Gen String
randString3 = do
  r0 <- randLetter
  r1 <- randLetter
  r2 <- randLetter
  return [r0,r1,r2]

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair g1 g2 = do
  r1 <- g1
  r2 <- g2
  return (r1,r2)

instance Monad Maybe where
  return = Just
  (Just x) >>= f = f x
  _ >>= f = Nothing

queryGreek :: GreekData -> String -> Maybe Double
queryGreek g s = do
  ints <- lookupMay s g
  int <- headMay ints
  double <- Just $ fromInteger int
  ints' <- tailMay ints
  maxInt <- maximumMay ints'
  maxDouble <- Just $ fromInteger maxInt
  maxDouble `divMay` double

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries l n1 n2 = do
  s1 <- lookupMay n1 l
  s2 <- lookupMay n2 l
  Just (s1 + s2)

tailProd :: Num a => [a] -> Maybe a
tailProd l = do
  t <- tailMay l
  return $ product t

tailSum :: Num a => [a] -> Maybe a
tailSum l = do
  t <- tailMay l
  return $ sum t

tailMax :: Ord a => [a] -> Maybe a
tailMax l = do
  t <- tailMay l
  maximumMay t

instance Monad [] where
  return x = [x]
  (>>=) = (concat .) . flip map

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = do
  x <- xs
  y <- ys
  return (x,y)

allCards :: [Int] -> [String] -> [Card]
allCards xs ys = do
  x <- xs
  y <- ys
  return $ Card x y


allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 f xs ys zs = do
  x <- xs
  y <- ys
  z <- zs
  return $ f x y z
