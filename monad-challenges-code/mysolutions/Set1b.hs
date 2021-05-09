{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1b where

import MCPrelude

type Gen a = Seed -> (a,Seed)

fiveRands :: [Integer]
fiveRands = let (r0,s0) = rand $ mkSeed 1
                (r1,s1) = rand $ mkSeed r0
                (r2,s2) = rand $ mkSeed r1
                (r3,s3) = rand $ mkSeed r2
                (r4,s4) = rand $ mkSeed r3
            in [r0,r1,r2,r3,r4]

randLetter :: Gen Char
randLetter s = let (i,s') = rand s
               in (toLetter i,s')

randString3 :: String
randString3 = let (r0,s0) = randLetter $ mkSeed 1
                  (q0,_) = rand $ mkSeed 1
                  (r1,s1) = randLetter $ mkSeed q0
                  (q1,_) = rand $ mkSeed q0
                  (r2,s2) = randLetter $ mkSeed q1
              in [r0,r1,r2]

randEven :: Gen Integer -- the output of rand * 2
randEven s = let (i,s') = rand s
             in (i*2,s')
randOdd :: Gen Integer -- the output of rand * 2 + 1
randOdd s = let (i,s') = randEven s
            in (i + 1,s')
randTen :: Gen Integer -- the output of rand * 10
randTen s = let (i,s') = rand s
            in (i*10,s')

--shouldBeTrue = 189908109902700 == (product $ map fst $ map ($ mkSeed 1) [randEven,randOdd,randTen])

generalA :: (a -> b) -> Gen a -> Gen b
generalA f g s = let (i,s') = g s
                 in (f i,s')

randEven' :: Gen Integer
randEven' = generalA (*2) rand
randOdd' :: Gen Integer
randOdd' = generalA (+1) randEven'
randTen' :: Gen Integer
randTen' = generalA (*10) rand

--shouldBeTrue' = 189908109902700 == (product $ map fst $ map ($ mkSeed 1) [randEven',randOdd',randTen'])

randPair :: Gen (Char, Integer)
randPair s = let (l,s') = randLetter s
                 (n,s'') = rand s'
             in ((l,n),s'')

generalPair :: Gen a -> Gen b -> Gen (a,b)
generalPair a b s = let (l,s') = a s
                        (n,s'') = b s'
                    in ((l,n),s'')

randPair_ :: Gen (Char, Integer)
randPair_ = generalPair randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f a b s = let (x,s') = a s
                       (y,s'') = b s'
                   in (f x y,s'')

repRandom :: [Gen a] -> Gen [a]
repRandom gs = \s -> foldl step ([],s) gs
  where
    step :: ([a],Seed) -> Gen a -> ([a],Seed)
    step (acc,s') g = let (x,s'') = g s'
                      in (reverse $ x:reverse acc,s'')

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo g f s = let (a,s') = g s
               in f a s'

mkGen :: a -> Gen a
mkGen a s = (a,s)

{- The following is from Set 4, actually -}
generalB2 :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB2 f a b = a `genTwo` (\a' -> b `genTwo` \b' -> mkGen (f a' b'))
-- in point-free style
generalB2' f a b = a `genTwo` ((b `genTwo`) . (mkGen .) . f)

-- the assignment says to re-implement repRandom in terms of
-- mkGen, generalA, and genTwo; however, given we've got
-- generalB (or generalB2, which is the same), we can
-- simply recognize that all repRandom' needs to do is
-- folding over its input using a lifted (:) as the folding
-- function, where the lifting is accomplished exactly via
-- generalB.
repRandom' :: [Gen a] -> Gen [a]
repRandom' = foldr (generalB2 (:)) (mkGen [])
