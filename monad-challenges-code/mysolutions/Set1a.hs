{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1a where

import MCPrelude

fiveRands :: [Integer]
fiveRands = let (r0,s0) = rand $ mkSeed 1
                (r1,s1) = rand $ mkSeed r0
                (r2,s2) = rand $ mkSeed r1
                (r3,s3) = rand $ mkSeed r2
                (r4,s4) = rand $ mkSeed r3
            in [r0,r1,r2,r3,r4]

randLetter :: Seed -> (Char, Seed)
randLetter s = let (i,s') = rand s
               in (toLetter i,s')

randString3 :: String
randString3 = let (r0,s0) = randLetter $ mkSeed 1
                  (q0,_) = rand $ mkSeed 1
                  (r1,s1) = randLetter $ mkSeed q0
                  (q1,_) = rand $ mkSeed q0
                  (r2,s2) = randLetter $ mkSeed q1
              in [r0,r1,r2]
