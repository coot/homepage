{-|
Module      : Data.FreeAlgebra
Description : Univeral Algebra approach to free algebras by examples https://marcinszamotulski.me/posts/free-monads.html.
Author      : Marcin Szamotulski, 2018
Copyright   : (c) Marcin Szamotulski, 2018
License     : MPL 2.0 (https://www.mozilla.org/en-US/MPL/2.0/)
Stability   : Experimental

This is a @GHC@ implementation of Universal Algebra approach to free algebras.
For a theoretical introduction to Universal Algebra and free algebras please
read [\"From Free Algebras to Free Monads\"](https://marcinszamotulski.me/posts/free-monads.html).
 -}
{-# OPTIONS_HADDOCK show-extensions     #-}

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExplicitForAll             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Data.FreeAlgebra where

import Prelude hiding (Monoid, mempty)

import           Control.Monad (ap, join)
import           Control.Monad.Free (Free)
import qualified Control.Monad.Free as Free
import           Data.Functor.Classes (Eq1 (..), Show1 (..), showsPrec1)
import           Data.Functor.Const (Const (..))
import           Data.Functor.Contravariant (Op (..))
import           Data.Functor.Coyoneda (Coyoneda (..), liftCoyoneda)
import           Data.Functor.Identity (Identity (..))
import           Data.Functor.Day (Day (..))
import qualified Data.Functor.Day as Day
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Proxy (Proxy (..))
import           GHC.Exts (Constraint)

-- * Algebra type and free algebras

-- |
-- We will use this type class to define constrains of some concrete algebras
-- @'a'@, e.g.
--
-- > instance Algebra [a] where
-- >    type AlgebraType [a] m = Monoid m
--
-- One could also do
--
-- > instance Monoid m => Algebra m where
-- >    type AlgebraType m n = Monoid m
--
-- but one would need to deal with overlapping instances for various types of
-- algebras: monoids ⊂ semigroups, etc; hence we will avoid it here.
type family AlgebraType (a :: k) (b :: l) :: Constraint

-- |
-- A free algebra of class @'AlgType'@.  An @'FreeAlgebra'@ constraint
-- proves that @'m'@ is a free algebra in the class of algebras of type
-- @'AlgType' m@ over the generators @'gen'@.
--
-- The functional dependecy helps to solve otherwise ambigous types in
-- @'hoistFree'@.
class FreeAlgebra (m :: * -> *)  where
    -- | Injective map that embeds generators of @m@.
    gen :: a -> m a
    -- | The freeness property.
    free :: forall d a .  AlgebraType m d
         => (a -> d)   -- ^ map generators of @m@ into @d@
         -> (m a -> d) -- ^ returns a homomorphism from @m@ to @d@

data FreeAlgebraT m a where
    FreeAlgebraT :: m a -> FreeAlgebraT m a

-- type instance AlgebraType (FreeAlgebraT m) (FreeAlgebraT m a) = ()
type instance AlgebraType (FreeAlgebraT m) n                  = AlgebraType m n

instance FreeAlgebra m => FreeAlgebra (FreeAlgebraT m) where
    gen                      = FreeAlgebraT . gen
    free f (FreeAlgebraT ma) = free f ma

-- |
-- All types which satisfy @'FreeAlgebra'@ constraint are foldable.
foldFree
    :: ( FreeAlgebra m
       , AlgebraType m a
       )
    => m a
    -> a
foldFree = free id

-- |
-- All types which satisfy @'FreeAlgebra'@ constraint are functors.
-- The constraint @AlgebraType m (m b)@ is always satisfied.
mapFree :: ( FreeAlgebra m
           , AlgebraType m (m b)
           )
        => (a -> b)
        -> m a
        -> m b
mapFree f ma = free (gen . f) ma

-- |
-- Further more all types which satisfy @'FreeAlgebra'@ constraint are moands.
joinFree :: ( FreeAlgebra m
            , AlgebraType m (m a)
            )
         => m (m a)
         -> m a
joinFree = foldFree

-- |
-- The cannonical quotient map from a free algebra of a wider class to a free
-- algebra of a narrower class, e.g. from free non-associative semigroup to
-- free associative semigroup, free monoid to free commutative monoid, etc.
--
-- > hoistFree . hoistFree == hoistFree
--
hoistFree
    :: forall m n a .
       ( AlgebraType m (n a) -- the algebra @n a@ is of the same type as algebra @m@
       , FreeAlgebra m       -- @m@ is free generated by @a@
       , FreeAlgebra n       -- @n@ is free generated by @a@
       )
    => m a
    -> n a
hoistFree = free gen

-- ** Non-Associative Semigroups

-- | Non-Associative (abbrev NA) semigroup bears `<>` method which is not
-- subject to any laws.
class NASemigroup a where
    -- associativity law is not assumed at this point
    (<>) :: a -> a -> a

-- |
-- Free non-associative semigroup generated by a type @a@.
data FreeNASemigroup a = Generator a | (FreeNASemigroup a) :*: (FreeNASemigroup a)
    deriving (Show, Eq, Ord, Functor)

instance Applicative FreeNASemigroup where
    pure = Generator
    (<*>) = ap

instance Monad FreeNASemigroup where
    return = pure
    m >>= f = joinNA $ f <$> m
        where
        joinNA :: FreeNASemigroup (FreeNASemigroup a) -> FreeNASemigroup a
        joinNA (Generator a) = a
        joinNA (a :*: b) = joinNA a :*: joinNA b

-- |
-- Defines @'AlgebraType'@ of @'FreeNASemigroup' a@ to be @'NASemigroup'@.
type instance AlgebraType FreeNASemigroup n               = NASemigroup n

-- |
-- @'FreeNASemigroup'@ is a free algebra.
instance FreeAlgebra FreeNASemigroup where
    gen = Generator
    free f (Generator a) = f a
    free f (a :*: b)     = free f a <> free f b

-- |
-- Non associative multiplication is just given by the @':*:'@ constructor.
instance NASemigroup (FreeNASemigroup a) where
    a <> b = a :*: b

foldNA :: NASemigroup a => FreeNASemigroup a -> a
foldNA = foldFree

newtype Conj = Conj { runConj :: Bool }
instance NASemigroup Conj where
    Conj True <> b = b
    _         <> _ = Conj False

newtype Disj = Disj { runDisj :: Bool }
instance NASemigroup Disj where
    Disj True <> _ = Disj True
    _         <> b = b

-- *** Examples
-- |
-- Non associative natural numbers:
--
-- > (), () :*: (), (() :*: ()) :*: (), () :*: (() :*: ()), ...
--
type FreeNASemigroup1 = FreeNASemigroup ()

-- | Free non associative semigroup with two generators.
type FreeNASemigroup2 = FreeNASemigroup (Sum ('Succ ('Succ 'Zero)) ())

-- ** Free non-associative commutative semigroup
-- Arbitrary finite products `GHC.TypeNats.+` is not an injective type family,
-- this is a problem for us, so let's define our own type level natural
-- numbers.

-- |
-- Type level natural numbers promoted with @DataKinds@.
data Nat = Zero | Succ Nat
    deriving (Show, Eq, Ord)

-- |
-- @'Nat'@ is a semigroup.
instance NASemigroup Nat where
    m <> Zero   = m
    m <> Succ n = Succ m <> n

-- |
-- n-fold products
data Prod (n :: Nat) a where
    Cons         :: a -> Prod n a -> Prod ('Succ n) a
    EmptyProduct :: Prod 'Zero a

deriving instance Show a => Show (Prod n a)
deriving instance Eq a => Eq (Prod n a)

p1 :: Prod ('Succ ('Succ ('Succ 'Zero))) [()]
p1 = Cons [()] (Cons [] (Cons [] EmptyProduct))

p2 :: Prod ('Succ ('Succ ('Succ 'Zero))) [()]
p2 = Cons [] (Cons [()] (Cons [] EmptyProduct))

p3 :: Prod ('Succ ('Succ ('Succ 'Zero))) [()]
p3 = Cons [] (Cons [] (Cons [()] EmptyProduct))

-- |
-- n-fold sum
data Sum (n :: Nat) a  where
    EmptySum :: Sum Zero a
    Add      :: Either (Sum n a) a -> Sum ('Succ n) a

deriving instance Show a => Show (Sum n a)
deriving instance Eq a => Eq (Sum n a)


s1 :: Sum ('Succ n) ()
s1 = Add (Right ())

s2 :: Sum ('Succ ('Succ n)) ()
s2 = Add (Left (Add (Right ())))

s3 :: Sum ('Succ ('Succ ('Succ n))) ()
s3 = Add (Left (Add (Left (Add (Right ())))))

-- |
-- A class for pointed types, ala pointed sets, i.e. types with a distinguished
-- term.  This is one of the most simple algebras, which homomorphism preserve
-- the distinguished element.
class Pointed m where
    mempty :: m -- ^ distinguished term

-- | @'Zero'@ is the distinguished term of type @'Nat'@.
instance Pointed Nat where
    mempty = Zero

instance Pointed (Maybe a) where
    mempty = Nothing

type instance AlgebraType Maybe p     = Pointed p

-- |
-- @'Maybe' a@ is the free pointed algebra generated by @a@.
instance FreeAlgebra Maybe where
    gen = Just
    free f Nothing = mempty
    free f (Just a) = f a

-- | @'Generator' ()@ is the distinguished term of type @'FreeNASemigroup1'@.
instance Pointed FreeNASemigroup1 where
    mempty = Generator ()

-- | @'EmptyProduct'@ is the distinguished term of @'Prod' \''Zero' m@.
instance Pointed (Prod 'Zero m) where
    mempty = EmptyProduct
-- |
-- This is diagonal build recursively by solving constraints which we could
-- not build directly in @'toProd'@.
instance (Pointed m, Pointed (Prod n m)) => Pointed (Prod ('Succ n) m) where
    mempty = Cons mempty mempty

-- |
-- Failed attempt to build the cannonical map from a sum to a product, we'll
-- solve this with a @'ToProduct'@ class.
toProd
    :: forall (x :: Nat)
     . Sum x ()
    -> Prod x FreeMonoid1
toProd EmptySum        = EmptyProduct
toProd (Add (Left s))  = Cons mempty (toProd s)
toProd (Add (Right _)) = Cons mempty undefined
    -- I'd like to put `mempty` here but ghc will not deduce that for
    -- `x ~ Succ y` `Prod y FreeMonoid1` is a `Monoid`
    -- we can go around this by using an inductive chain of instances

-- In category theory there is a unique morphism from a (disjoint) sum `a ∪ b`
-- to a product `a × b`.  This type class let us construct it inductively.
class ToProduct (n :: Nat) m a b where
    toProduct :: (a -> b) -- ^ how to transform coeffcients
              -> (m n a)  -- ^ a sum type
              -> Prod n b -- ^ the resulting product

instance ToProduct 'Zero Sum a b where
    toProduct _ _ = EmptyProduct
instance ( Pointed m
         , Pointed (Prod n m)
         , ToProduct n Sum () m
         ) =>
         ToProduct ('Succ n) Sum () m where
    toProduct f (Add (Left s))  = Cons mempty (toProduct f s)
    toProduct f (Add (Right a)) = Cons (f a) mempty

-- |
-- Construct any @'Prod'@ with all values equal in CPS style.
diagonal :: Nat -> a -> (forall n. Prod n a -> r) -> r
diagonal Zero _ f     = f EmptyProduct
diagonal (Succ n) a f = diagonal n a (f . Cons a)

instance NASemigroup a => NASemigroup (Prod n a) where
    EmptyProduct <> EmptyProduct = EmptyProduct
    Cons a as <> Cons b bs       = Cons (a <> b) (as <> bs)

-- ** Associative Semigroups

-- |
-- Class for associative semigroups, for which:
--
-- >  (a <> b) <> c = a <> (b <> c)
--
class NASemigroup m => Semigroup m

instance Semigroup Nat
instance Semigroup Conj
instance Semigroup Disj
instance Semigroup a => Semigroup (Prod n a)

-- |
-- Free semigroup
newtype FreeSemigroup a = FreeSemigroup { runFreeSemigroup :: NonEmpty a }
    deriving (Eq, Ord, Show, Functor)

type instance AlgebraType FreeSemigroup m = Semigroup m

instance NASemigroup (FreeSemigroup a) where
    FreeSemigroup (a :| as) <> FreeSemigroup (b :| bs)
        = FreeSemigroup $  a :| as ++ b : bs
instance Semigroup (FreeSemigroup a)

-- |
-- Proof that @'FreeSemigroup' a@ is a free semigroup generated by @a@.
instance FreeAlgebra FreeSemigroup where
    gen a = FreeSemigroup $ a :| []
    free f (FreeSemigroup (a :| [])) = f a
    free f (FreeSemigroup (a :| (a' : as')))
        = f a <> free f (FreeSemigroup $ a' :| as')

foldNonEmpty :: Semigroup a => FreeSemigroup a -> a
foldNonEmpty = foldFree

-- | Free (associative) semigroup with one generator, which is commutative.
type FreeSemigroup1 = FreeSemigroup ()
-- | Free (associative) semigroup with two generators, hence non commutative.
type FreeSemigroup2 = FreeSemigroup (Either () ())

-- |
-- This map is the cannonical epimorphism from the free non-associative
-- semigroup to free associative semigroup.  It is uniquely determined on
-- generators.
--
-- >>> :set -XTypeApplications
-- >>> runFreeSemigroup $ toAssociative $ Generator () :*: (Generator () :*: Generator ())
-- () :| [(),()]
-- >>> runFreeSemigroup $ toAssociative $ (Generator () :*: Generator ()) :*: Generator ()
-- () :| [(),()]
toAssociative :: forall a. FreeNASemigroup a -> FreeSemigroup a
toAssociative = hoistFree

-- ** Commutative semigroups
-- |
-- Class of commutative semigroups, i.e. such that
--
-- > a <> b = b <> a
class Semigroup m => CommutativeSemigroup m

-- |
-- >>> Succ Zero <> Succ (Succ Zero) == Succ (Succ Zero) <> Succ Zero
-- True
instance CommutativeSemigroup Nat
instance CommutativeSemigroup Conj
instance CommutativeSemigroup Disj
instance CommutativeSemigroup FreeSemigroup1
instance CommutativeSemigroup m => CommutativeSemigroup (Prod n m)

-- * Monoids
class (Semigroup m, Pointed m) => Monoid m

instance Monoid Nat

-- |
-- @'UndecidableInstances'@ is required here since the constraint is no
-- smaller than the instance head.
instance (Monoid m, Pointed (Prod n m)) => Monoid (Prod n m)

newtype FreeMonoid a = FreeMonoid { runFreeMonoid :: [a] }
    deriving (Eq, Ord, Show, Functor)

type instance AlgebraType FreeMonoid m = Monoid m

instance NASemigroup (FreeMonoid a) where
    FreeMonoid as <> FreeMonoid bs
        = FreeMonoid $  as ++ bs
instance Semigroup (FreeMonoid a)

-- | An empty list @[]@ is the distinguished term of @'FreeMonoid'@.
instance Pointed (FreeMonoid a) where
    mempty = FreeMonoid []
instance Monoid (FreeMonoid a)

instance FreeAlgebra FreeMonoid where
    gen :: a -> FreeMonoid a
    gen a = FreeMonoid [a]

    free :: Monoid d
         => (a -> d)
         -> FreeMonoid a
         -> d
    free f (FreeMonoid [])       = mempty
    free f (FreeMonoid (a : as)) = f a <> free f (FreeMonoid as)

foldMonoid :: Monoid m => FreeMonoid m -> m
foldMonoid = foldFree

toMonoid :: FreeSemigroup a -> FreeMonoid a
toMonoid = hoistFree

-- | It is isomorphic with @'Nat'@ (as an additive monoid) via
-- @'natToFreeMonoid1'@.
type FreeMonoid1 = FreeMonoid ()
instance CommutativeSemigroup FreeMonoid1

type FreeMonoid2 = FreeMonoid (Sum ('Succ ('Succ 'Zero)) ())
type FreeMonoid3 = FreeMonoid (Sum ('Succ ('Succ ('Succ 'Zero))) ())

f1 :: FreeMonoid3
f1 = FreeMonoid [s1]

f2 :: FreeMonoid3
f2 = FreeMonoid [s2]

f3 :: FreeMonoid3
f3 = FreeMonoid [s3]

-- * Commutative monoids

-- ** Free commutative monoid
-- The free commutative monoid is (Prod n FreeMonoid1) (with @n@ generators).

type instance AlgebraType (Prod n FreeMonoid1) m =
    ( Monoid m
    , CommutativeSemigroup m
    )

type instance AlgebraType Nat m = ( Monoid m, CommutativeSemigroup m )

{--
  - -- |
  - -- A proof that @'Nat'@ is free commutative semigroup.
  - instance FreeAlgebra Nat where
  -     gen _ = Zero
  -     free f (Succ m) = free f Zero <> free f m
  --}

{--
  - instance FreeAlgebra (Prod 'Zero FreeMonoid1) (Sum 'Zero ()) where
  -     gen :: Sum 'Zero () -> Prod 'Zero FreeMonoid1
  -     gen = toProduct (\a -> [a])
  -
  -     free f EmptyProduct = f EmptySum
  -
  - instance ( Pointed (Prod (n) FreeMonoid1)
  -          , ToProduct ('Succ n) Sum () FreeMonoid1
  -          , FreeAlgebra (Prod n FreeMonoid1) (Sum n ())
  -          )
  -          => FreeAlgebra (Prod ('Succ n) FreeMonoid1) (Sum ('Succ n) ()) where
  -
  -     gen :: Sum ('Succ n) () -> Prod ('Succ n) FreeMonoid1
  -     gen = toProduct (\a -> [a])
  -
  -     free :: forall n d
  -           . ( Monoid d
  -             , CommutativeSemigroup d
  -             , Pointed (Prod n FreeMonoid1)
  -             , FreeAlgebra (Prod n FreeMonoid1) (Sum n ())
  -             )
  -          => (Sum ('Succ n) () -> d)
  -          -> (Prod ('Succ n) FreeMonoid1)
  -          -> d
  -     free f (Cons a as) = free @FreeMonoid1 g a <> free @(Prod n FreeMonoid1) h as
  -         where
  -         g :: () -> d
  -         g _ = f (Add (Right ()))
  -         h :: Sum n () -> d
  -         h s = f (Add (Left s))
  --}

{--
  - -- |
  - -- Isomorphism from @'Nat'@ to @'FreeMonoid1'@ usign @'hoistFree'@.
  - --
  - -- > freeMonoid1ToNat . natToFreeMonoid1 = id
  - -- > natToFreeMonoid1 . freeMonoid1ToNat = id
  - natToFreeMonoid1 :: Nat -> FreeMonoid1
  - natToFreeMonoid1 nat = case coerce nat of
  -         Cons fm1 _ -> fm1
  -     where
  -         coerce :: Nat -> Prod ('Succ 'Zero) FreeMonoid1
  -         coerce = hoistFree (Proxy :: Proxy (Sum ('Succ 'Zero) ()))
  -
  - -- |
  - -- Inverse of @'natToFreeMonoid1'@ usign @'hoistFree'@.
  - freeMonoid1ToNat :: FreeMonoid1 -> Nat
  - freeMonoid1ToNat fm1 = coerce (Cons fm1 EmptyProduct)
  -     where
  -         coerce :: Prod ('Succ 'Zero) FreeMonoid1 -> Nat
  -         coerce = hoistFree (Proxy :: Proxy (Sum ('Succ 'Zero) ()))
  --}

{--
  - -- |
  - -- The cannonical epimorphism from a free monoid onto free commutative monoid,
  - -- obtained using @'free'@.  The compiler will solve all the constraints for
  - -- any concrete type of kind @'Nat'@.
  - --
  - -- >>> abelianization f1
  - -- Cons [()] (Cons [] (Cons [] EmptyProduct))
  - -- >>> abelianization (f1 <> f2) == abelianization f1 <> abelianization f2
  - -- True
  - -- >>> abelianization (f1 <> f1)
  - -- Cons [(),()] (Cons [] (Cons [] EmptyProduct))
  - abelianization
  -     :: forall (n :: Nat)
  -      . ( Monoid (Prod n FreeMonoid1)
  -        , ToProduct n Sum () FreeMonoid1
  -        , FreeAlgebra (Prod n FreeMonoid1) (Sum  n ())
  -        )
  -     => FreeMonoid (Sum n ())
  -     -> Prod n FreeMonoid1
  - abelianization = hoistFree (Proxy :: Proxy (Sum n ()))
  -
  - type FreeAbMonoid1 = Prod ('Succ 'Zero) FreeMonoid1
  - type FreeAbMonoid2 = Prod ('Succ ('Succ 'Zero)) FreeMonoid1
  - type FreeAbMonoid3 = Prod ('Succ ('Succ ('Succ 'Zero))) FreeMonoid1
  --}


-- * Higher Kinded Algebras

type family AlgebraType1 (m :: k) (b :: l) :: Constraint

-- |
-- Higher kinded version of @'FreeAlgebra'@.  Instances includes free functors,
-- free applicative functors and free monads.
class FreeAlgebra1 (m :: (* -> *) -> * -> *) where
    -- | Natural transformation that embeds generators into @m@.
    gen1 :: AlgebraType1 m f => f a -> m f a

    -- | The freeness property.
    free1 :: forall (d :: * -> *) f a .
             ( AlgebraType m d
             , AlgebraType1 m f
             )
          => (forall x. f x -> d x)
          -- ^ natural transformation which embeds generators of @m@ into @d@
          -> (m f a -> d a)
          -- ^ a homomorphism from @m@ to @d@

foldFree1 :: ( FreeAlgebra1 m
             , AlgebraType m f
             , AlgebraType1 m f
             )
          => m f a
          -> f a
foldFree1 = free1 id

{--
  - mapFree1 :: forall m f a b .
  -             ( FreeAlgebra1 m
  -             , AlgebraType1 m (m f)
  -             , AlgebraType1 m f
  -             )
  -          => (a -> b)
  -          -> m f a
  -          -> m f b
  - mapFree1 f mfa = free1' f' mfa
  -     where
  -         free1' :: (forall x . f x -> m f x) -> m f a -> m f b
  -         free1' fn = free1 fn
  -         f' = gen1 . fmap f
  --}

-- |
-- Analogous to @'hoistFree'@
--
-- > hoistFree1 . hoistFree1 = hoistFree1
hoistFree1
    :: forall m n f a .
       ( AlgebraType m (n f)
       , AlgebraType1 m f
       , AlgebraType1 n f
       , FreeAlgebra1 m
       , FreeAlgebra1 n
       )
    => m f a
    -> n f a
hoistFree1 = free1 gen1

-- |
-- Algebra type for a functor.
type instance AlgebraType Coyoneda g  = Functor g
type instance AlgebraType1 Coyoneda g = ()

-- |
-- Proof that @'Coyoneda' f@ is a free functor.
--
-- >>> gen1 (Identity 'a') == Coyoneda id (Identity 'a')
-- True
--
-- It satisfies:
--
-- > free1 id == lowerCoyoneda
--
-- >>> :{
--     let a = gen1 @Coyoneda (Identity 'a')
--     in free1 @Coyoneda @Identity id a
-- >>> :}
-- Identity 'a'
--
-- >>> :{
--     let a = gen1 @Coyoneda (Op (const 0))
--     in free1 @Coyoneda @(Const Integer) (\(Op fn) -> Const $ fn undefined) a
-- >>> :}
-- Const 0
instance FreeAlgebra1 Coyoneda where
    gen1 :: f a -> Coyoneda f a
    gen1 = liftCoyoneda

    free1 :: Functor g
          => (forall x. f x -> g x)
          -> Coyoneda f a
          -> g a
    free1 nat (Coyoneda ba fb) = ba <$> nat fb

foldCoyoneda :: Functor f => Coyoneda f a -> f a
foldCoyoneda = free1 id

-- |
-- Free applicative functor.  See [Free Aplicative
-- Functors](https://arxiv.org/pdf/1403.0749.pdf) by
-- P.Capriotti and A.Kaposi.
data FreeA f a where
    Pure :: a -> FreeA f a
    FreeA :: f (x -> a) -> FreeA f x -> FreeA f a

instance Functor f => Functor (FreeA f) where
    fmap f (Pure a)          = Pure (f a)
    fmap fab (FreeA fxa ffx) = FreeA (fmap fab <$> fxa) ffx

instance Functor f => Applicative (FreeA f) where
    pure = Pure

    Pure fab      <*> fa = fab <$> fa
    FreeA fxab fx <*> fa = FreeA (fmap uncurry fxab) ((,) <$> fx <*> fa)

-- |
-- Algebra type for an applicative functor.
type instance AlgebraType FreeA g  = Applicative g
type instance AlgebraType1 FreeA g = Functor g

-- |
-- A proof that @'FreeA'@ is Free applicative monad.
--
-- >>> free1 id (gen1 @FreeA (Identity 'a'))
-- Identity 'a'
instance FreeAlgebra1 FreeA where
    gen1 :: Functor f => f a -> FreeA f a
    gen1 fa = FreeA (const <$> fa) (Pure fa)

    free1 :: forall (d :: * -> *) f a .
             ( Functor f
             , Applicative d
             )
          => (forall x. f x -> d x)
          -> (FreeA f a -> d a)
    free1 _   (Pure a) = pure a
    free1 nat (FreeA fxa fx) = nat fxa <*> free1 nat fx


-- |
-- >>> foldFreeA (FreeA (Identity (+1)) (Pure 0))
-- Identity 1
foldFreeA :: Applicative f => FreeA f a -> f a
foldFreeA = foldFree1

coyonedaToFreeA :: forall f a . Functor f => Coyoneda f a -> FreeA f a
coyonedaToFreeA = hoistFree1

-- ** Day convolution
-- Note that for @'Day' f g@ to be an applicative functor it is not enough that
-- @f@ and @g@ are functors, one needs to assume that both are applicative.
-- Hence @'Day' f f@ is not a candidate for a free applicative functor
-- generated by a functor @f@.  Nevertheless it bears @'FreeAlgebra1'@
-- property whenever @f@ is applicative functor.

newtype DayF f a = DayF { runDayF :: Day f f a }
    deriving (Functor, Applicative)

-- |
-- @'DayF' f@ has the same algebra type as @'FreeA' f@.
type instance AlgebraType DayF g  = Applicative g
type instance AlgebraType1 DayF g = Functor g

instance FreeAlgebra1 DayF where
    gen1 :: Functor f => f a -> DayF f a
    gen1 fa = DayF $ Day fa fa (\a _ -> a)

    free1 :: ( Functor f
             , Applicative d
             )
          => (forall x. f x -> d x)
          -> DayF f a
          -> d a
    free1 nat (DayF day) =  Day.dap . Day.trans2 nat . Day.trans1 nat $ day

-- |
-- Note that this morphism requires @f@ to be @'Applicative'@ rather than just
-- a @'Functor'@, like @'coyonedaToFreeA'@.
coyonedaToDay :: forall f a . Applicative f => Coyoneda f a -> DayF f a
coyonedaToDay = hoistFree1

-- |
-- Split epimorphism with right inverse @'dayToFreeA'@, i.e.
--
-- > freeAToDay . dayToFreeA = id
freeAToDay :: forall f a . Applicative f => FreeA f a -> DayF f a
freeAToDay = hoistFree1

-- |
-- Left inverse of @'freeAToDay'@ (split monomorhism).
dayToFreeA :: forall f a . Applicative f => DayF f a -> FreeA f a
dayToFreeA = hoistFree1

-- ** Free monads

-- |
-- Algebra type for a monad.
type instance AlgebraType Free m  = Monad m
type instance AlgebraType1 Free f = Functor f

-- |
-- A proof that @'Free' f@ is a free monad.
--
-- > free1 id == Free.retract
--
-- >>> free1 id (gen1 @Free (Identity 1))
-- Identity 1
instance FreeAlgebra1 Free where
    gen1 :: Functor f => f a -> Free f a
    gen1 = Free.liftF

    free1 :: (Functor f, Monad d)
          => (forall x. f x -> d x)
          -> Free f a
          -> d a
    free1 = Free.foldFree

foldFree_ :: Monad m => Free m a -> m a
foldFree_ = foldFree1

freeAToFree :: forall f a . Functor f => FreeA f a -> Free f a
freeAToFree = hoistFree1

-- ** Freer monads

-- |
-- Freer monad is a composition of @'Free'@ and @'Coyoneda'@.  We only require
-- that the generators @f@ are of kind @* -> *@ but not necessarily a functor.
newtype Freer f a = Freer { runFreer :: Free (Coyoneda f) a }
    deriving (Functor, Applicative, Monad)

type instance AlgebraType Freer m  = Monad m
type instance AlgebraType1 Freer f = ()

-- |
-- @'Freer'@ monad is a @'FreeAlgebra1'@ over @f :: * -> *@ which not
-- necessarilly is a functor.
instance FreeAlgebra1 Freer where
    gen1 fa = Freer $ gen1 $ Coyoneda id fa
    free1 :: forall f d a. Monad d
          => (forall x. f x -> d x)
          -> Freer f a
          -> d a
    free1 _ (Freer (Free.Pure a)) = return a
    free1 nat (Freer (Free.Free (Coyoneda bfca fb))) =
        let db = nat fb
        in db >>= (\ b -> free1 nat $ Freer $ bfca b)

foldFreer :: Monad m => Freer m a -> m a
foldFreer = foldFree1

coyonedaToFreer :: forall f a . Coyoneda f a -> Freer f a
coyonedaToFreer = hoistFree1

-- |
-- Iso with inverse @'freeToFree'@, since @'Coyoneda' f@ is isomorphic to @f@
-- when @f@ is a functor.
freeToFreer :: forall f a . Functor f => Free f a -> Freer f a
freeToFreer = hoistFree1

freerToFree :: forall f a . Functor f => Freer f a -> Free f a
freerToFree = hoistFree1

-- * Algebras

-- |
-- A uniform way of defining algebras.  For example the `Algebra (FreeMonoid m)
-- m` instance is a proof that a monoid is an `FreeMonoid-algebra` (in the
-- sense of f-algebras).  Inverse is also true, by monadicity of category of
-- monoids.  Hence having a `fold :: [m] -> m` which respects the laws of
-- `FreeMonoid-algebras` is equivalent to having a monoid.
--
class Algebra f a where
    fold :: (FreeAlgebra f, AlgebraType f a) => f a -> a

instance NASemigroup a => Algebra FreeNASemigroup a where
    fold :: NASemigroup a => FreeNASemigroup a -> a
    fold = foldFree

instance Semigroup a => Algebra FreeSemigroup a where
    fold :: Semigroup a => FreeSemigroup a -> a
    fold = foldFree

instance Monoid m => Algebra FreeMonoid m where
    fold :: Monoid m => FreeMonoid m -> m
    fold = foldFree

-- * Higher Kinded Algebras

class Algebra1 (m :: (* -> *) -> * -> *) (f :: * -> *) where
    fold1 :: FreeAlgebra1 m => m f a -> f a

instance Functor f => Algebra1 Coyoneda f where
    fold1 = foldFree1

instance Applicative f => Algebra1 FreeA f where
    fold1 = foldFree1

instance Monad m => Algebra1 Free m where
    fold1 = foldFree1

instance Monad m => Algebra1 Freer m where
    fold1 = foldFree1