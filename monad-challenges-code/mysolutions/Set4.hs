{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set4 where

import MCPrelude
import Set2

fmap :: (a -> b) -> f a -> f b
fmap = undefined

liftA2 {- `generalB` and `yLink` -} :: (a -> b -> c) -> f a -> f b -> f c
liftA2 = undefined

(>>=) {- `genTwo` and `link` -} :: m a -> (a -> m b) -> m b
(>>=) = undefined

join :: m (m a) -> m a
join = undefined


class Monad m where
  bind {- `genTwo` and `link`, or (>>=) -} :: m a -> (a -> m b) -> m b
  return {- `mkGen` and `Just` -} :: a -> m a

instance Monad Maybe where
  bind (Just x) f = f x
  return = Just

instance Monad [] where
  bind = (concat .) . flip map
  return x = [x]

newtype Gen a = Gen { run :: Seed -> (a,Seed) }

instance Monad Gen where
  return a = Gen $ \s -> (a, s)
  bind g f = Gen $ \s -> let (a,s') = run g s
                          in run (f a) s'

evalGen :: Gen a -> Seed -> a
evalGen g s = a where (a,_) = run g s
