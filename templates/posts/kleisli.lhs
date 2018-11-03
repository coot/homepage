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

Kleisli contstruction
---------------------

> newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
> 
> instance Monad m => Category (Kleisli m) where
>   id = Kleisli return
>   Kleisli f . Kleisli g = Kleisli (\a -> g a >>= f)

Free monads
-----------

Free monad construction gives you a monad out of a functor.  It will satisfy
all the monad laws: associativity, and unitality axioms, and it is a universal
construction.

> data Free f a
>   = Free (f (Free f a))
>   | Return a
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
(laws).  The first is that both `liftFree` and `foldNatFree nat` (for some
`nat`) are natural transformation, butin addition `foldNatFree` should be
a morphism of monads, i.e.

-- TODO: graph

> instance FreeAlgebra1 Free where
>   liftFree fa = Free (Return <$> fa)
>   foldNatFree _nat (Return a) = return a
>   foldNatFree nat  (Free ff)  =
>     join $ nat $
>       (foldNatFree nat) <$> ff -- induction step

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

This means that `Kleisli (Free f)` is a free category for the class of Kleisli
categories, not for all categories though.  There is some math left still to
be able to claim this.  We should be able to show that `liftKleisli` and
`foldKleisli nat` (for any natural transformation `nat :: forall x. f x ->
m x`) are functors.  This will follow from the laws that `liftFree` and
`foldNatFree` satisfy.

-- TODO: proofs
