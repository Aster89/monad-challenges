{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  MCPrelude
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  mightybyte@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- MCPrelude is a modified version of the Haskell Prelude designed
-- specifically for The Monad Challenges.
--
-----------------------------------------------------------------------------

module MCPrelude (

    -- * Standard types, classes and related functions

    -- ** Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise,

    Either(Left, Right),
    either,

    Ordering(LT, EQ, GT),
    Char, String,

    -- *** Tuples
    fst, snd, curry, uncurry,

    -- ** Basic type classes
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- ** Numbers

    -- *** Numeric types
    Int, Integer, Float, Double,
    Rational,

    -- *** Numeric type classes
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- *** Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,

    -- ** Miscellaneous functions
    id, const, (.), flip, ($), until,
    asTypeOf, error,
    seq, ($!),

    -- * List operations
    map, (++), filter,
    null, length, (!!),
    reverse,
    -- ** Reducing lists (folds)
    foldl, foldl1, foldr, foldr1,
    -- *** Special folds
    and, or, any, all,
    sum, product,
    concat, concatMap,
    maximum, minimum,
    -- ** Building lists
    -- *** Scans
    scanl, scanl1, scanr, scanr1,
    -- *** Infinite lists
    iterate, repeat, replicate, cycle,
    -- ** Sublists
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    -- ** Searching lists
    elem, notElem,
    -- ** Zipping and unzipping lists
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    -- ** Functions on strings
    lines, words, unlines, unwords,

    -- * Converting to and from @String@
    -- ** Converting to @String@
    ShowS,
    Show(showsPrec, showList, show),
    shows,
    showChar, showString, showParen,
    -- ** Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,

  ) where

import Data.List
import Data.Either
import Data.Tuple

import GHC.Base
import Text.Read
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.Show

infixr 0 $!

-- -----------------------------------------------------------------------------
-- Miscellaneous functions

-- | Strict (call-by-value) application, defined in terms of 'seq'.
($!)    :: (a -> b) -> a -> b
f $! x  = let !vx = x in f vx  -- see #2273

#ifdef __HADDOCK__
-- | The value of @'seq' a b@ is bottom if @a@ is bottom, and otherwise
-- equal to @b@.  'seq' is usually introduced to improve performance by
-- avoiding unneeded laziness.
seq :: a -> b -> b
seq _ y = y
#endif
