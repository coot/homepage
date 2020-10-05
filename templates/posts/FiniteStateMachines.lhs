Typed Transitions, Finite State Machines and Free Categories
============================================================

In this post we will explore __finite state machines__ with typed transitions
represented as finite directed graphs via __free categories__.  You will also
see how usefull is the __Kleisli category__.

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE InstanceSigs #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeApplications #-}

> module FiniteStateMachines where

> import           Prelude hiding (id, foldMap, (.))
> import           Control.Category (Category (..), (<<<))
> import           Control.Monad ((>=>))
> import           Data.List.NonEmpty (NonEmpty (..), (<|))
> import           Data.Void (Void)
> import           Unsafe.Coerce (unsafeCoerce)

Free Category Construction
--------------------------

A _free category_ generated by a (directed) _graph_ is formed by adding identity
edges to each vertex and then taking the graph of all possible paths, i.e
every path in this graph, becomes arrow in the generated category.  The final
step is to impose category theory laws: so that the added identity arrows
satisfy the unit law.  Path composition is always associative, so at least
this we get for free.
Composition of arrows is just composition of paths.  Note that this
construction correponds exactly to the construction of the _free monoid_: if
you take a graph with a single vertex `*` and a bunch of edges from `*` to
`*` then the _free monoid_ generated by this set of edges is the same as the
free category (every monoid can be seen as a category with a single object).

> data Cat :: (* -> * -> *) -> * -> * -> * where
>     Id :: Cat f a a
>     (:.:) :: f b c -> Cat f a b -> Cat f a c
>
> instance Category (Cat f) where
>     id = Id
>     Id . ys = ys
>     (x :.: xs) . ys = x :.: (xs . ys)

Let us check that the category theory laws holds.  First let us observe that
by recursive definition of `Cat`, every element has a form: `(f1 :.: (f2 :.: (
... :.: Id)))`.

![normal form of a morphism in Cat f](/images/free-cat-morphism.svg)

The smallest $n$ such that a morphism has this form we call the length of
a morphism.

First unit law: `Id . x = x` holds by definition; to show `x . Id = x`, it's
enough to consider the case when x has length greater than 1:

```
(x :.: xs) . Id
   == x :.: (xs . Id)
   -- by induction on the length of xs
   == x :.: xs
``` 

Now let us prove associativity. The proof is also by induction on the length
of the first element:

```
((x :.: Id) . y) . z
  == (x :.: y) . z
  == (x :.: (y . z))
  == (x :.: (Id . (y . z))
  == (x :.: Id) . (y . z)
```

And the induction step:

```
((x :.: xs) . y) . z
  == (x :.: (xs . y)) . z
  ==  x :.: ((xs . y) . z)
  -- by induction on the length of xs
  ==  x :.: (xs . (y . z))
  ==  (x :.: xs) . (y . z)
```

As expected we have a lawful category `Cat f`.

For each `a` we have an embedding:

> endo :: [f a a] -> Cat f a a
> endo [] = Id
> endo (x : xs) = x :.: endo xs

As all the free constructions, also free category has the lift operation which
let you embed the generating graph into the free category generated by it.
It is a generalisation of the singleton list

```(: []) :: a -> [a]```

and at the same time `lift` for monad transformers.

> liftCat :: f a b -> Cat f a b
> liftCat fab = fab :.: Id

Being a free category means that whenever you have a binatural transformation
from `f :: * -> * -> *` to a category `Category g => g :: * -> * -> *` you can
construct (in a unique way) a functor from `Cat f` to `g`.  This is the spirit
of free algebras.  And indeed we can get a `foldMap` like map:

> foldFunCat
>     :: forall f g a b . Category g
>     => (forall x y. f x y -> g x y)
>     -- ^ a map of graphs
>     -> (Cat f a b -> g a b)
>     -- ^ a functor from 'Cat f' to 'g'
> foldFunCat _ Id = id
> foldFunCat fun (bc :.: ab)
>   = fun bc <<< foldFunCat fun ab

This is a free constructions in the sense I've been advocating for some time in
a series of blog posts:
[from free algebras to free
monads](https://marcinszamotulski.me/posts/free-monads.html),
[monadicity](https://marcinszamotulski.me/posts/monadicity.html), based on 
[free-algebras](https://hackage.haskell.org/package/free-algebras) package
(published on hackage).

The Kleisli Category
-------------------

in `Control.Arrow` there is the following construction, which is attributed to
a Swiss category theorist [Heinrich
Kleisli](https://www.genealogy.math.ndsu.nodak.edu/id.php?id=18361).  It turns
out that with any moand `m` one can associate a category where arrows are `a
-> m b` instead of `a -> b`.  Let us investigate this in detail, as this
allows for many interesting interpretations.

> newtype Kleisli m a b =
>    Kleisli { runKleisli :: a -> m b }
> 
> instance Monad m => Category (Kleisli m) where
>   id                    = Kleisli return
>   Kleisli f . Kleisli g = Kleisli (g >=> f)

The arrow

```
(>=>) :: (a -> m b) -> (b -> m c) -> a -> m c
(f >=> g) a = f a >>= g
```

is called Kleisli composition (or if you prefer using `join`: `\f g a -> join
$ fmap g (f a)`).  Monadic operations `return` and
`>>=` carry the unitality laws:

```
return >>= f == f
m >>= return == m
```


They become even simpler when we re-write them using `>=>`:

```
return >=> f == f
f >=> return == f
```

This means that `Kleisli return` is indeed the identity arrow in `Kleisli m`
category.  It remain to show that the composition is associative, and this, as
you can expect, can be derived from the monad associativity law:

```
m >>= (\x -> k x >>= h)
  == (m >>= k) >>= k)
```

which using Kleisli composition, takes much simpler form (which conveys the
reason for the name of this axiom):

```f >=> (g >=> h)
  == (f >=> g) >=> h
```

Let us prove this:

```
(f >=> (g >=> h)) a 
  == f a >>= (g >=> h)
  == f a >>= \b -> g b >>= h)
  -- by monadic associativity law
  == (f a >>= g) >>= h
  == ((f >=> g) a) >>= h
  == ((f >=> g) >=> h) a
```

The associativity of Kleisli composition `>=>` is exactly what we need to
prove associativity of `Kleisli m` category, so we're done!  This is the one
of rare cases when using point free style makes the presentation look much
easier to read ;).

Also note that there is a functor from `(->)` category to `Kleisli m` given by

> arr :: Monad m => (a -> b) -> Kleisli m a b
> arr f = Kleisli $ return . f

It is a part of the `Monad m => Arrow (Kleisli m)` instance in
`Control.Arrow` module of the
[base](https://hackage.haskell.org/package/base-4.11.1.0/docs/Control-Arrow.html#t:Kleisli)
package.

There is a worth noting sepcialization of `foldFunCat` to Kleisli category:

> foldFunKleisli
>    :: forall f m a b . Monad m
>    => (forall x y. f x y -> Kleisli m x y)
>    -> (Cat f a b -> Kleisli m a b)
> foldFunKleisli = foldFunCat

if you expand `Kleisli` newtype wrapper we will get

> foldFunKleisli'
>   :: Monad m
>   => (forall x y.  f x y -> x -> m y)
>   -> Cat f a b
>   -> a -> m b
> foldFunKleisli' fun cab = runKleisli $ foldFunKleisli (Kleisli . fun) cab

A final observation, is that in any category the type `cat c => c v v` is
a monoid with identity `id` and multiplication `(.)`.  In `(->)` we
have `Data.Monoid.Endo` newtype wrapper for that purpose, and it could be
generalised:

> data Endo c a where
>   Endo :: Category c => c a a -> Endo c a
>
> instance Semigroup (Endo c a) where
>   Endo f <> Endo g = Endo (f <<< g)
>
> instance Category c => Monoid (Endo c a) where
>   mempty = Endo id

This includes `Endo (Kleisli m) a ≅ a -> m a` as an example (for a monad `m`).
If you try to prove the associativity and unit laws for this monoid, you'll
discover that what you need is associativity and unit laws for monad.

Example: bifunctor with a single object
---------------------------------------

As an example let us consder a bifunctor with a single object:

> data Single e v a b where
>   Single      :: e -> Single e v v v
>   VoidSingle  :: Void -> Single e v a b

With `Single` you can only construct terms of type `Single e v v v`, any other
term diverge.  We need `VoidSingle` constructor to provide a `Category` type
class instance.

In this case `endo` is an isomorphism with inverse (modulo `Single
e v v v ≅ e`):

> toList :: Cat (Single e v) v v -> [e]
> toList Id                  = []
> toList (Single e :.: es)    = e : toList es
> toList (VoidSingle e :.: _) = undefined

Whenever `e` is a `Monoid`, `Single e v` is a `Category`:

> idSingle :: Monoid e => Single e v v v
> idSingle = Single mempty
>
> composeSingle
>    :: Monoid e
>    => Single e v b c
>    -> Single e v a b
>    -> Single e v a c
> composeSingle (Single a)     (Single b)     = Single (a <> b)
> composeSingle (VoidSingle _) _              = undefined
> composeSingle _              (VoidSingle _) = undefined

> instance Monoid e => Category (Single e v) where
>   id :: forall a . Monoid e => Single e v a a
>   id  = unsafeCoerce (idSingle @e)
>   (.) = composeSingle

Furthemore, in this case the free category corresponds to free
monoid; `Cat (Single e v)` is a single object category with

```Cat (Single e v) v v ≅ [e]```

the free monoid generated on type `Single e v v v ≅ e`.

![category (Cat (Single e v)) with one object v](/images/one-object-cat.svg)

We will show now that `foldFunCat` in this case is nothing than a `foldMap`:

> foldMap :: Monoid m => (a -> m) -> [a] -> m
> foldMap _ [] = mempty
> foldMap f (a : as) = f a <> foldMap f as

First let us see how `foldFuncCat` specializes:

> _foldFunCat
>     :: forall e f v a b .
>        Monoid e
>     => (forall x y . f x y -> Single e v x y)
>     -> Cat f a b
>     -> Single e v a b
> _foldFunCat = foldFunCat

now note that the only natural transformation `f x y -> Single e v x y` that we
can have are one that comes from a map `g :: f v v -> Single e v v v`.  Hence
`foldFunCat` reduces further to to

> foldFunCat'
>     :: forall e f v.
>        Monoid e
>     => (f v v -> Single e v v v) -- ≅ f v v -> e
>     -> Cat f v v                 -- ≅ [f v v]
>     -> Single e v v v            -- ≅ e
> foldFunCat' f c = foldFunCat (unsafeCoerce f) c

Assuming that `endo :: f v v -> Cat f v v` is an isomorphism (which it is for
a large class of bifunctors, e.g. `Single e v`) we have: `Cat f v v ≅ [v]`; so
we end up with a map `Monoid m => (a -> m) -> [a] -> m` which is the claimed
`foldMap`.  Finally, both `foldMap` and `foldFunCat` are defined using the
same recursion pattern, hence they must be equal.  

To recap what we have just show: `foldFunCat` for
`f = Single e v` and `g = Monoid m => Single e v` is just `foldMap`.
In this case we can view `foldFunCat` as a generalisation `foldMap`.  There
is also another way of coming to this conclusin via free objects (check out
[free-algebras package](https://hackage.haskell.org/package/free-algebras).

Example Finite State Machine
----------------------------

For this post I picked the example of a state machine explored by Oscar
Wickström in his short series about state machines:
[part 1](https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html)
and
[part 2](https://wickstrom.tech/finite-state-machines/2017/11/19/finite-state-machines-part-2.html).
It is a simple state transition for an online shop.  I slightly simplified it,
by making the assumption that one can cancel at any stage
(just for presentation purposes).

States (vertices of the FSM):

> data NoItems       = NoItems
> newtype HasItems   = HasItems (NonEmpty CartItem)
> newtype NoCard     = NoCard (NonEmpty CartItem)
> data CardSelected  = CardSelected Card (NonEmpty CartItem)
> data CardConfirmed = CardConfirmed Card (NonEmpty CartItem)
> data OrderPlaced   = OrderPlaced

The shop only sells unit objects (better than seling `Void` terms ;) )

> type CartItem = ()

Accepted credit cards:

> type Card = String

The FTM's directed graph can be described by a type of kind `* -> * -> *`,
where first type is the source of an arrow, the second type is its target.
Directed graph lack composition, and we will fix this.  In this example we
take (after [Oscar
Wickström](https://wickstrom.tech/finite-state-machines/2017/11/10/finite-state-machines-part-1-modeling-with-haskell.html),
though here `Cancel` can abort at any stage rather than just during
confirmation, just for simplicity):

> data Tr s t where
>     SelectFirst :: CartItem -> Tr NoItems HasItems
>     Select      :: CartItem -> Tr HasItems HasItems
>     SelectCard  :: Card -> Tr HasItems CardSelected
>     Confirm     :: Tr CardSelected CardConfirmed
>     PlaceOrder  :: Tr CardConfirmed OrderPlaced
>     Cancel      :: Tr s NoItems

Category generated by the `Tr` graph.

![Tr graph](/images/tr-graph.svg)

> type ShoppingCat a b = Cat Tr a b

As a graph `ShoppingCat` has the same vertices as `Tr`, but has more edges.
Any path that you can follow in the `Tr` graph becomes a new edge in
`ShoppingCat`, e.g. `SelectFirst` followed by `Select` is a new edge from
`NoItems` to `HasItems`.  Note that at this point we don't have any
interpretation of the arrows, we only modeled the shape of the category we
want to have.  This gives us freedom how to interpret this category in other
categories using functors (not to confuse with `Functor` instances: these are
endofunctors of `(->)`).

Interpretation of the `Tr` graph in the `(->)` category:

> natPure :: Tr a b -> a -> b
> natPure (SelectFirst i) _                    = HasItems (i :| [])
> natPure (Select i)      (HasItems is)        = HasItems (i <| is)
> natPure (SelectCard c)  (HasItems is)        = CardSelected c is
> natPure Confirm         (CardSelected c is)  = CardConfirmed c is
> natPure PlaceOrder      _                    = OrderPlaced
> natPure Cancel          _                    = NoItems

Interpretation of `ShoppingCat` in `(->)` (a functor between two categories):

> checkoutPure :: ShoppingCat a b -> a -> b
> checkoutPure = foldFunCat natPure

But we can easily interpret in `ShoppingCat` in any Kleisli category,
especially in `Klesli IO`.  Here we lift just the pure interpretation, but
equaly well you could do some `IO` here.

> checkoutM
>     :: forall m a b . Monad m
>     => ShoppingCat a b
>     -> Kleisli m a b
> checkoutM = foldFunCat nat
>     where
>     nat :: Tr x y -> Kleisli m x y
>     nat xy = arr $ natPure xy

Unpacking the `Kleisli` category gives us:

> chechoutM' :: Monad m => ShoppingCat a b -> a -> m b
> chechoutM' = runKleisli . checkoutM

The freedom of the choice of monad in the Kleisli category can give you
various ways of dealing with exceptional conditions (e.g. not valid card) and
error handling (`IOException`s ...).  Also having various interpretation can
be very good for testing, e.g. having a reference implementation might be
a very good idea to increase assurance of the software you are developing.
Check out Duncan Coutts' <a
href="https://www.youtube.com/watch?v=mhKUHpQZIoc">lecture</a> on this
technique.

Finally tagless description
---------------------------

We can give a finally tagless description of the shopping category.  For that
we first define the class of categories in which one can do all the `Tr`
operations:

> class Category c => ShoppingCatT (c :: * -> * -> *) where
>     selectFirst :: CartItem -> c NoItems HasItems
>     select      :: CartItem -> c HasItems HasItems
>     selectCard  :: Card     -> c HasItems CardSelected
>     confirm     :: c CardSelected CardConfirmed
>     placeOrder  :: c CardConfirmed OrderPlaced
>     cancel      :: c s NoItems
>
> instance ShoppingCatT (Cat Tr) where
>     selectFirst = liftCat . SelectFirst
>     select      = liftCat . Select
>     selectCard  = liftCat . SelectCard
>     confirm     = liftCat Confirm
>     placeOrder  = liftCat PlaceOrder
>     cancel      = liftCat Cancel

There is a unique functor `embed :: ShopingCatT c => ShoppingCat a b -> c a b`
which with preserves all the operations, e.g.

```
embed (SelectFirst i) = selectFirst i 
embed (Select i)      = select i
embed (SelectCard v)  = selectCard v
embed Confirm         = confirm
embed PlaceOrder      = placeOrder
embed Cancel          = cancel
```

This property does not leave any space how this functor has to be
implemented, that's why `ShoppingCat` is the initial `ShoppingCatT` category.

> embed :: forall c a b. ShoppingCatT c
>       => ShoppingCat a b
>       -> c a b
> embed = foldFunCat nat
>     where
>     nat :: Tr x y -> c x y
>     nat (SelectFirst i) = selectFirst i
>     nat (Select i)      = select i
>     nat (SelectCard c)  = selectCard c
>     nat Confirm         = confirm
>     nat PlaceOrder      = placeOrder
>     nat Cancel          = cancel

Complete graph with a single vertex
-----------------------------------

Let us go back to the `Single e v` graph.

A graph is [complete](https://en.wikipedia.org/wiki/Complete_graph) if every
two vertices are connected by a unique edge.  It may also happen that all the
vertices can be represented by a single type `a`.  Then the whole theory
collapses to a category with a single object, i.e. a monoid (as we discovered
earlier for the `Single e v` graph).  In this case the generating graph can
also be reduced to just a single type (usually a sum of
all possible events).  In this case one can describe the state machine simply
by a free monoid `[e]` where `e` represents the type of events and use the
following version of `foldMapKleisli` (`foldFunCat`) to give interpretations:

> foldMapKleisli :: Monad m
>                => (e -> Kleisli m v v)
>                -> [e]
>                -> Kleisli m v v
> foldMapKleisli _ [] = id
> foldMapKleisli f (e : es) = f e <<< foldMapKleisli f es

The first argument of `foldMapKleisli` maps events to (monadic) state
transformations.  You can model pure transformations with `Kleisli Identity`
(`Kleisli Identity a v ≅ v -> v`),
or you might want to use `IO` with `Kleisli IO` (`Kleisli IO v v ≅ v -> IO
v`).

And again, what you are seeing here is `foldMap`, this is simply because
`Kleisli m v v` is a monoid (as every type `Category c => cat a a` is).  The
composition is given by `<<<` and `mempty` is the identity arrow `id`, so the
above formula corresponds to `foldMap`.  This is the very special case if your
state machine can be represented as a single object category, i.e. a monoid.
