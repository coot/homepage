module Data.Algebra.Sum where

import qualified Data.Functor.Sum as Functor (Sum (..))
-- ** Free sum

class FreeSum (c :: * -> Constraint) (m :: * -> * -> *) where
    first   :: (c a, c b) => a -> m a b
    second  :: (c a, c b) => b -> m a b
    -- |
    -- Lawful instance must ensure that the map @'freeSum' f g@ is unique.
    freeSum :: (c x, c a, c b)  => (a -> x) -> (b -> x) ->  m a b -> x

bimapFreeSum
    :: forall m c a b x y .
       ( FreeSum c m
       , c (m x y)
       , c (m a b)
       )
    => (a -> x)
    -> (b -> y)
    -> m a b
    -> m x y
bimapFreeSum f g m = freeSum @c @m (first @c @m . f) (second @c @m . g) m

data FSemigroupS a b
    = SFFirst a
    | SFSecond b
    | a :<>: b

instance (NASemigroup a, NASemigroup b) => NASemigroup (FSemigroupS a b) where
    SFFirst a <> SFFirst a' = SFFirst $ a <> a'
    SFFirst a <> SFSecond b = a :<>: b
    SFSecond b <> SFFirst a = a :<>: b
    SFSecond b <> SFSecond b' = SFSecond $ b <> b'
    SFFirst a <> (a' :<>: b) = (a <> a') :<>: b
    (a :<>: b) <> SFFirst a' = (a <> a') :<>: b   
    SFSecond b <> (a :<>: b') = a :<>: (b <> b')
    (a :<>: b) <> SFSecond b' = a :<>: (b <> b')
    (a :<>: b) <> (a' :<>: b') = (a <> a') :<>: (b <> b')

-- |
-- Note that this is truelly associative instance
--
-- @
-- ((a1 :<>: b1) <> (a2 :<>: b2)) <> (a3 :<>: b3)
--      = ((a1 <> a2) :<>: (b1 <> b2)) <> (a3 :<>: b3)
--      = (((a1 <> a2) <> a3) :<>: ((b1 <> b2) <> b3)
--      = ((a1 <> (a2 <> a3)) :<> (b1 <> (b2 <> b3))
--      = (a1 :<>: b1) <> ((a2 <> a3) :<>: (b2 <> b3))
--      = (a1 :<>: b1) <> ((a2 :<>: b2) <> (a3 :<>: b3))
-- @
instance (Semigroup a, Semigroup b) => Semigroup (FSemigroupS a b)

instance FreeSum NASemigroup FSemigroupS where
    first  = SFFirst
    second = SFSecond
    freeSum f _ (SFFirst a)  = f a
    freeSum _ g (SFSecond b) = g b
    freeSum f g (a :<>: b)   = f a <> g b

instance FreeSum Semigroup FSemigroupS where
    first  = SFFirst
    second = SFSecond
    freeSum f _ (SFFirst a)  = f a
    freeSum f g (a :<>: b)   = f a <> g b


-- * higher kinded free sums

class FreeSum1 (c :: (* -> *) -> Constraint) (m :: (* -> *) -> (* -> *) -> * -> *) where
    inL :: forall x f g. c f => f x -> m f g x
    inR :: forall x f g. c g => g x -> m f g x
    either1 :: forall f g h a
             . ( c g, c g, c h )
            => (forall x. f x -> h x)
            -> (forall x. g x -> h x)
            -> m f g a
            -> h a

instance FreeSum1 Functor Functor.Sum where
    inL = Functor.InL
    inR = Functor.InR
    either1 f _ (Functor.InL fx) = f fx
    either1 _ g (Functor.InR gx) = g gx
