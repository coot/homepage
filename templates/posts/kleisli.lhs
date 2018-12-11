---
layout: post
title: "Typed Transitions, Finite-State Machines and Free Categories"
date: 25-08-2018
author: Marcin Szamotulski
categories: finite-state-machines
tags: ["haskell", "finite-state-machines", "categories", "math"]
excerpt: |
    Finite state machines are graphs, which generate a category.  We explore
    this point of using free algebra approach and tagless-final style.
---

Kleisli categories and Free monads
==================================

Given a monad __m__ one can construct the _Kleisli category_
`Control.Arrow.Kleisli m`, in this post we'll explore what happens when we
start with a free monad.

> {-# LANGUAGE KindSignatures      #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE RankNTypes          #-}

> module Control.Category.Kleisli where
>
> import Prelude hiding (id, (.))
> import Control.Category (Category (id, (.)))
> import Control.Monad (ap, join)
> import qualified Control.Monad.Free as Free

Kleisli contstruction
---------------------

> newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
> 
> instance Monad m => Category (Kleisli m) where
>   id = Kleisli return
>   Kleisli f . Kleisli g = Kleisli (\a -> f =<< g a)

The Keisli composition is so useful, it has it's own operator defined in
`Control.Arrow`:

> (<=<) :: Monad m => (y -> m z) -> (x -> m y) -> x -> m z
> f <=< g = \x -> f =<< g x

Free monads
-----------

Free monad construction gives you a monad out of a functor.  It will satisfy
all the monad laws: associativity, and unitality axioms, and it is a universal
construction.

> data Free f a
>   = Return a
>   | Free (f (Free f a))
>
> instance Functor f => Functor (Free f) where
>   fmap f (Return a) = Return (f a)
>   fmap f (Free ff)  = Free (fmap f <$> ff)
>
> instance Functor f => Applicative (Free f) where
>   pure = return
>   (<*>) = ap
>
> instance Functor f => Monad (Free f) where
>   return = Return
>   Return a >>= f = f a
>   Free ff  >>= f = Free (fmap (>>= f) ff)

The universal property of a free monad can be expressed with a class, which
I copy from `free-algebras` package (I don't include details that are not
important for this blog post):

> class FreeAlgebra1 (m :: (* -> *) -> * -> *) where
>   -- | Natural transformation that embeds generators into `m`.
>   liftFree :: Functor f => f a -> m f a
> 
>   -- | The freeness property.
>   foldNatFree
>     :: forall d f a .
>        ( Monad d
>        , Functor f
>        )
>     => (forall x. f x -> d x)
>     -- ^ a natural transformation which embeds generators of `m` into `d`
>     -> (m f a -> d a)
>     -- ^ a morphism from `m f` to `d`

> instance FreeAlgebra1 Free where
>   liftFree fa = Free (Return <$> fa)
>   foldNatFree _nat (Return a) = return a
>   foldNatFree nat  (Free ff)  =
>     join $ nat $
>       (foldNatFree nat) <$> ff -- induction step

Instances of this class have the property that to construct a natural
transformation from `FreeAlgebra1 m => m f` to a monad `Monad d => d` is
enought to come up with a natural transformation of functors `forall x. f a ->
d a`.  If you'd like to explore more why this class is the right one to speak
about freeness, checkout one of my previous
[posts](https://coot.me/posts/free-monads.html).  Note that the instance
follows the structure, and there is not much room how to implement the
methods: that's a very common feeling in category theory, which means one is
on the right path.

A valid instance of `FreeAlgebra1` has to have some additional properties
or laws (as they are called more often).  The first is that both `liftFree`
and `foldNatFree nat` (for any `nat`) are natural transformations:

![liftFree natural transformation](/images/liftFree-nat.svg)

or as an equation: `fmap g . liftFree == liftFree . fmap g`

![foldNatFree natural transformation](/images/foldNatFree-nat.svg)

or as an equation:

```
fmap g . foldNatFree nat
  == foldNatFree nat . fmap g
```

There are also an additional requirement, which comes from the fact that we
are looking for a free functor from the category of (endo-)functors into the
category of monads.  `foldNatFree nat` has to ba monad morphism for any `nat`.
This means that the following diagrams commute (or equations hold):

![foldNatFree - monad morphism: the unit law](/images/foldNatFree-mmorph-unit.svg)

or as equations: `return = return . foldNatFree nat` and more two commutative
diagrams:

![foldNatFree - monad morphism: the join law](/images/foldNatFree-mmorph-join.svg)

or as an equation:

```
join . (foldNatFree nat . fmap (foldNatFree nat))
  == foldNatFree nat . join
```

Note that by the natural transformation law for `foldNatFree` we have:

```
foldNatFree nat . fmap (foldNatFree nat)
  == fmap (foldNatFree nat) . foldNatFree nat
```

For any monad morphism `fn :: (Monad m, Monad n) => m a -> n a`, one can easily
prove that `fn . (f <=< g) == fn . f <=< fn . g`, in particular this is true for
`foldNatFree nat`.

```
(fn . f <=< fn . g)
  == \a -> fn . f =<< (fn . g) a
  == \a -> fn . f =<< fn (g a)
  -- by definition of join (or =<< in terms of a join)
  == \a -> join (fn . f <$> fn (g a))
  -- by functor associativity law
  == \a -> join (fn <$> (f <$> fn (g a)))
  -- since fn is a morphism of monads it is a natural transformation
  -- and thus it commutes with fmap/<$>
  == \a -> join (fn <$> (fn (f <$> g a)))
  -- since fn is a morphism of monads: join (fmap fn . fn) == fn . join
  == \a -> fn (join (f <$> g a))
  -- by definition of join
  == \a -> fn (f =<< g a)
  == \a -> fn (f <=< g) a
  == fn (f <=< g)
```

The proof could be much shorter if we use monad morphism law in terms of
binds.  The equivalne form of `join (fmap fn . fn) == fn . join` expressed
with bind is `fn (f =<< ma) = (fn . f) =<< fn ma`.  Another reason to use
`join` is that the law take the same form as for monoid homomorphisms.

Kleisli categories for free monads
----------------------------------

> liftKleisli
>   :: Monad m
>   => Kleisli m a b
>   -> Kleisli (Free m) a b
> liftKleisli (Kleisli f) = Kleisli (liftFree . f)

> foldKleisli
>   :: ( Functor f
>      , Monad m
>      )
>   => (forall x. f x -> m x)
>   -> Kleisli (Free f) a b
>   -> Kleisli m a b
> foldKleisli nat (Kleisli f) = Kleisli $ foldNatFree nat . f

This means that `Kleisli (Free f)` is a free category for the class of graphs
of type `Functor f => Kleisli f` (`Keisli f` is a category when `f`
is a monad).  Both morphisms: `liftKleisli` is marely a morphisms of graphs,
while `foldKleisli` is a functor, which means it preserves `id` and the
composition `(.) :: Category c => c y z -> c x y -> c x y`.

```
foldKleisli nat id
  == foldKleisli nat (Kleisli Return)
  == Kleisli (foldNatFree nat . Return)
  == Kleisli return
  == id
```

```
foldKleisli nat (Kleisli f . Kleisli g)
  == foldKleisli nat (Kleisli f <=< g)
  == Kleisli (foldNatFree nat (f <=< g))
  -- foldNatFree nat is a morphism of monads thus
  == Kleisli (foldNatFree nat f <=< foldNatFree nat g)
  == Kleisli (foldNatFree nat f) . Kleisli (foldNatFree nat g)
  == foldKleisli nat f . foldKleisli nat g
```
