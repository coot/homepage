{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE RankNTypes           #-}

module Main where

import           Control.Applicative (liftA2)
import           Data.Bifunctor (Bifunctor, first)
import           Data.Monoid (Monoid (..))
import           Data.Functor.Classes (Eq1 (..), eq1)
import           Data.Functor.Identity (Identity (..))

-- Free monoid with a single generator (it's also free in the class of abelian
-- (commutative) monoids). It is isomorphic to `[()]`.
data Nat = Zero
         | Succ Nat
    deriving (Eq, Ord, Show)

instance Monoid Nat where
    mempty = Zero
    Zero     `mappend` m = m
    (Succ n) `mappend` m = n `mappend` (Succ m)

-- Free monoid with two generators `SuccX` and `SuccY`.  It is isomorphic to
-- `[Bool]`.
data Nat2 = Zero2
          | SuccX Nat2
          | SuccY Nat2
    deriving (Eq, Ord, Show)

instance Monoid Nat2 where
    mempty = Zero2
    Zero2     `mappend` m = m
    (SuccX n) `mappend` m = n `mappend` (SuccX m)
    (SuccY n) `mappend` m = n `mappend` (SuccY m)

-- Free abelian (commutative) monoid with two generators is just `(Nat, Nat)`.

-- The free monad is a type which is an umbrella for all the terms:
-- a; f a; f (f a); …
data Free f a = Return a
              | Free (f (Free f a))
    deriving (Functor)

instance Applicative f => Applicative (Free f) where
    pure = Return
    (Return f) <*> fa = f <$> fa
    (Free ff)  <*> (Free fa)  = Free $ liftA2 (<*>) ff fa

-- join for the `Free f` monad: stihing trees of trees to get a tree
joinFree :: Functor f => Free f (Free f a) -> Free f a
joinFree (Return fa) = fa -- stich tree of trees
joinFree (Free ff)   = Free (joinFree <$> ff)

instance Applicative f => Monad (Free f) where
    return = Return
    fa >>= f = joinFree $ f <$> fa

join :: Monad m => m (m a) -> m a
join mma = mma >>= id

foldFree :: (Functor f, Monad m)
         => (forall x. f x -> m x)
         -> Free f a -> m a
foldFree _ (Return a) = return a
foldFree f (Free fa)  = join $ f $ foldFree f <$> fa

foldMapL :: Monoid m => (a -> m) -> [a] -> m
foldMapL _ [] = mempty
foldMapL f (a : as) = mappend (f a) (foldMapL f as)

instance Eq1 f => Eq1 (Free f) where
    liftEq eq = go
        where
        go (Return a) (Return b) = eq a b
        go (Free fa)  (Free fb)  = liftEq go fa fb
        go _          _          = False

-- The free monad with one generator.
data Free1 a = Free1 Nat a
    deriving (Eq, Ord, Show, Functor)

instance Applicative Free1 where
    pure a = Free1 Zero a
    Free1 n f <*> Free1 m a = Free1 (n `mappend` m) (f a)

instance Monad Free1 where
    return a = pure a
    Free1 n a >>= f = case f a of
        Free1 m a' -> Free1 (n `mappend` m) a'

-- `Free Identity ≅ Free1` as monads!
-- `toFree1 . fromFree1 = id`
-- `fromFree1 . toFree1 = id`
toFree1 :: Free Identity a -> Free1 a
toFree1 (Return a)           = Free1 Zero a
toFree1 (Free (Identity fa)) = case toFree1 fa of
    Free1 n a -> Free1 (Succ n) a

fromFree1 :: Free1 a -> Free Identity a
fromFree1 (Free1 Zero     a) = Return a
fromFree1 (Free1 (Succ n) a) = Free (Identity (fromFree1 (Free1 n a)))

-- |
-- `MSet Nat ~ Free1 ~ Free Identity`, but in general this is not a free
-- monad.  `MSet m a` is the free `m-set` for a monoid `m`.
data MSet m a = MSet m a

instance Functor (MSet m) where
    fmap f (MSet m a) = MSet m (f a)

instance Monoid m => Applicative (MSet m) where
    pure a = MSet mempty a
    MSet n f <*> MSet m a = MSet (n `mappend` m) (f a)

instance Monoid m => Monad (MSet m) where
    return a = pure a
    MSet n a >>= f = case f a of
        MSet m a' -> MSet (m `mappend` n) a'

data F2 a = FX a | FY a
    deriving Functor

data S2 = SX | SY

toM2 :: Free F2 a -> ([S2], a)
toM2 (Return a) = ([], a)
toM2 (Free (FX fa)) = case toM2 fa of
    (m, a) -> (SX : m, a)
toM2 (Free (FY fa)) = case toM2 fa of
    (m, a) -> (SY : m, a)

fromM2 :: ([S2], a) -> Free F2 a
fromM2 ([], a) = Return a
fromM2 (SX : xs, a) = Free (FX (fromM2 (xs, a)))
fromM2 (SY : xs, a) = Free (FY (fromM2 (xs, a)))

-- Exercise show that `Free X2 ≌ MSet Nat2`; or generalise this too `Free Xn
-- ≌ MSet Natn` (where `n` is the number of generators).

newtype Const a b = Const { runConst :: a }
    deriving Functor

toEither :: Free (Const a) b -> Either a b
toEither (Return b) = Right b
toEither (Free (Const a)) = Left a

fromEither :: Either a b -> Free (Const a) b
fromEither (Right b) = Return b
fromEither (Left a)  = Free (Const a)

main :: IO ()
main = return ()
