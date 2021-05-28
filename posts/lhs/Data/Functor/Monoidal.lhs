Monoidal and Applicative Functors
=================================

In this posts we will explore the equivalence between applicative and monoidal
functors (i.e. functors which preserve cartesian product).

<div style="display: none;">
  IDEA: make a framework, which typechecks all the proofs; maybe use liquid
  haskell for that.
</div>

> module Data.Functor.Monoidal where

> import           Data.Functor (($>))
> import           Data.Functor.Identity (Identity (..))

Monoidal categories
-------------------

We will only focus on a basic example of a monoidal category.  The
prototypical example is category with products, e.g. the category `Hask`
together with `(,)` type, and this is the category we will consider.
A monoidal category also assumes a unit object and some laws for the monoidal
product which look very familiar to those of a monoid, but slightly more
general.  A monoidal structure in a given category is a bit like a monoid,
where you can combine any two objects of a category but the unitality and
associativity laws have to be relaxed. They are satisfied up to an
isomorphism.  This is needed even in the simplest case as we have here where
the monoidal product is simply given by the categorial product, i.e. the pair
type `(a,b)`.  

_associativity_

> assoc :: (a, (b, c)) -> ((a, b), c)
> assoc (a, (b, c)) = ((a, b), c)

_left unit_

> leftUnit :: a -> ((), a)
> leftUnit a = ((), a)

_right unit_

> rightUnit :: a -> (a, ())
> rightUnit a = (a, ())

And these isomorphism have to satisfy additional, so called coherence
conditions.  This is all done for a greater generality, where one wants the
monoidal product to keep reasonable simple.  Since we're not focused here on
full introduction of monoidal categories, and we won't need its coherence laws
we will not dive into the details.

There are monoidal categories which monoidal product is different than the
product, and this is probably more natural for many application in pure
mathematics, especially in algebra (tensor product in linear algebra, smashed
product in some topological context, or the category of endofunctors, ...).

Monoidal functors - definition
------------------------------

Since we now have a `Hask` as an example of a monoidal category, we will be
interested in functors that preserve this monoidal structure.  They are called
monoidal functors, and they are defined by the following type class.

> class Functor f => Monoidal f where
>   unit  :: f ()
>   (<|>) :: f a -> f b -> f (a, b)
>
> infixl 4 <|>

Note that this definition expressese the notion of a monoidal endo-functors of
the category `(->)` in which the monoidal product is given by the tuple type
`(a, b)`.

Monoidal functors - laws
------------------------

Monoidal functors have to obey the following laws:

_left unit law_
```haskell
unit <|> fb   = (unit,) <$> fb
```
_right unit law_
```haskell
fa  <|> unit  = (,unit) <$> fa
```

_naturality_

```haskell
(f <$> fa) <|> fb = (\(a,b) -> (f a, b)) <$> (fa <|> fb)
fa <|> (f <$> fb) = (\(a,b) -> (a, f b)) <$> (fa <|> fb)
```

Thus

```haskell
unit <|> unit = (unit,) <$> unit = (,unit) <$> unit
```

Applicative functors
--------------------

There's no need to introduce
[applicative
functors](https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Applicative).
Let me just cite the applicative functor laws, which we will use quite
extensievly:

_identity law_
```haskell
pure id <*> v = v
```

_composition law_
```haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

_homomorphism law_
```haskell
pure f <*> pure x = pure (f x)
```

_interchange law_
```haskell
u <*> pure y = pure ($ y) <*> u
```

We will now present an argument that _every applicative functor_ is
a functor for which the following _formula_ holds:

```haskell
fmap f fa = pure f <*> fa
```

<div class="lemma">
  **Lemma**
  If `f` is an applicative functor then

> liftA :: Applicative f => (a -> b) -> f a -> f b
> liftA f fa = pure f <*> fa

satisfies functor laws.
</div>

**Proof**
```haskell
liftA id fa
  = pure id <*> fa
  -- by identity law
  = fa
```

```haskell
liftA (f . g) fa
  = pure (f . g) <*> fa
  = pure ((.) f g) <*> fa
  = pure (.) <*> pure f <*> pure g <*> fa
  -- by composition law
  = pure f <*> (pure g <*> fa)
  = liftA f (liftA g fa)
```
<span class="qed">QED</span>

Any data structure has at most one functor instance, thus whenever one has
a functor instance it must be the one.  As a consequence for an applicative
functor `fmap` and `liftA` are always equal.  This allows us to use `liftA` in
exchange for `fmap` in proofs.  That's very handy, since otherwise the
applicative properties do not provide any compatiblity with `fmap`.

It turns out that every applicative functor is a monoidal one.  And this is
thanks to the following standard definition of `monoidal` and a bit further
down `monoidalUnit`:

> -- | Every applicative functor is monoidal.
> --
> monoidal :: Applicative f
>          => f a
>          -> f b
>          -> f (a, b)
> monoidal fa fb = (,) <$> fa <*> fb

> monoidalUnit :: Applicative f
>              => f ()
> monoidalUnit = pure ()


> instance Monoidal Identity where
>   unit  = monoidalUnit
>   (<|>) = monoidal
> 
> instance Monoidal IO where
>   unit  = monoidalUnit
>   (<|>) = monoidal

But we still need to prove the monoidal laws.

**Monoidal laws of `monoidal` and `monoidalUnit`**

_left unit law_
```haskell
monoidalUnit `monoidal` fb
  = (,) <$> (pure ()) <*> fb
  = pure (,) <*> pure () <*> fb
  -- by homomorphism law
  = pure ((),) <*> fb
  = ((),) <$> fb
```

_right unit law_
```haskell
fa `monoidal` monoidalUnit
  = (,) <$> fa <*> pure ()
  -- by interchange law
  = pure ($ ()) <*> ((,) <$> fa)
  = pure ($ ()) <*> (pure (,) <*> fa)
  -- by composition law
  = pure (.) <*> pure ($ ()) <*> pure (,) <*> pure fa
  = pure ((.) ($ ()) (,)) <*> fa
  = pure (\(a -> (a, ())) <*> fa
  = pure (,()) <*> fa
  = (,()) <$> fa
```

<div class="lemma">
  **Lemma**

```haskell
(\a b -> (f a, b)) <$> fa <*> fb
  = (\(a, b) -> (f a, b)) <$> ((,) <$> fa <$> fb)
```
</div>

**Proof**
It's probably not a surprise that we will need to use applicative composition
law.  A first useful observation is that
```haskell
(\a b -> (f a, b))
  = (\g -> (\(a, b) -> (f a, b)) . g) . (\a b -> (a, b))
  = ((.) (\(a, b) -> (f a, b))) . (\a b -> (a, b))
```

```haskell
(\a b -> (f a, b)) <$> fa <*> fb
  -- by the above observation
  = (((.) (\(a, b) -> (f a, b))) . (\a b -> (a, b))) <$> fa <*> fb
  -- by functor law
  = ((.) (\(a, b) -> (f a, b))) <$> ((\a b -> (a, b)) <$> fa) <*> fb
  = pure ((.) (\(a, b) -> (f a, b))) <*> ((\a b -> (a, b)) <$> fa) <*> fb
  -- by applicative homomorphism law
  = pure (.) <*> pure (\(a, b) -> (f a, b)) <*> ((\a b -> (a, b)) <$> fa) <*> fb
  -- by applicative composition law
  = pure (\(a, b) -> (f a, b)) <*> (((\a b -> (a, b)) <$> fa) <*> fb)
  -- by applicative functor lemma
  = (\(a, b) -> (f a, b)) <$> ((,) <$> fa <$> fb)
```
<span class="qed">QED</span>

_naturality laws_
```haskell
(f <$> fa) `monoidal` fb
  = (,) <$> (f <$> fa) <*> fb
  = (,) <$> (pure f <*> fa) <*> fb
  = pure (,) <*> (pure f <*> fa) <*> fb
  -- by composition law
  = pure (.) <*> pure (,) <*> pure f <*> fa <*> fb
  -- by functor lemma
  = pure ((.) (,) f) <*> fa <*> fb
  = pure (\a b -> (f a, b)) <*> fa <*> fb
  = (\a b -> (f a, b)) <$> fa <*> fb
  = (\(a, b) -> (f a, b)) <$> ((\a b -> (a, b)) <$> fa <*> fb)
  -- by previous lemma
  = (\(a, b) -> (f a, b)) <$> ((,) <$> fa <*> fb)
  = (\(a, b) -> (f a, b)) <$> (fa `monoidal` fb)
```

```haskell
fa `monoidal` (f <$> fb)
  = (,) <$> fa <*> (f <$> fb)
  = ((,) <$> fa) <*> (pure f <*> fb)
  -- by composition law
  = pure (.) <*> ((,) <$> fa) <*> pure f <*> fb
  = (.) <$> ((,) <$> fa)) <*> pure f <*> fb
  -- by functor law
  = (.) . (,) <$> fa <*> pure f <*> fb
  -- by interchange law
  = pure ($ f) <*> ((.) . (,) <$> fa) <*> fb
  = pure ($ f) <*> (pure ((.) . (,)) <*> fa) <*> fb
  -- by composition law
  = pure (.) <*> pure ($ f) <*> pure ((.) . (,)) <*> fa <*> fb
  = pure ((.) ($ f) ((.) . (,))) <*> fa <*> fb
  = pure (\(a,b) -> (a, f b)) <*> fa <*> fb
  = (\a b -> (a, f b)) <$> fa <*> fb
  = (\(a, b) -> (a, f b)) . (,) <$> fa <*> fb)
  -- by composition law
  = (\(a, b) -> (a, f b)) <$> ((,) <$> fa <*> fb)
```


From Monoidal to Applicative functors
-------------------------------------

> -- | And conversely every monoidal functor is applicative.
> --
> monoidalAp :: Monoidal f
>            => f (a -> b)
>            -> f a
>            -> f b
> monoidalAp fab fa =
>   uncurry ($) <$> (fab <|> fa)

> monoidalPure :: Monoidal f
>              => a
>              -> f a
> monoidalPure a = unit $> a

**Applicative laws of `monoidalAp` and `monoidalPure`**

_homomorphism law_
```haskell
 monoidalPure ab `monoidalAp` monoidalPure a
  = uncurry ($) <$> ((unit $> ab) <|> (unit $> a))
  = uncurry ($) <$> ((const ab <$> unit) <|> ((const a <$> unit)))
  = uncurry ($) <$> (\_ -> (ab, a)) <$> (unit <|> unit)
  = uncurry ($) . (\_ -> (ab, a)) <$> (unit <|> unit)
  = const (ab a) <$> (unit <|> unit)
  by the fact that that (const a) <$> f == (const b) <$> g for any
  `f, g :: f a`
  = const (ab a) <$> unit
  by definition of ($>)
  = unit $> (ab a)
  = monoidalPure (ab a)
```
<span class="qed">QED</span>

_identity law_
```haskell
 monoidalPure id `monoidalAp` f
  = uncurry ($) <$> ((unit $> id) <|> f)
  by /naturality/ (and the definition of @'$>'@
  = uncurry ($) <$> (\(_, b) -> (id, b)) <$> (unit <|> f)
  = uncurry ($) . (\(_, b) -> (id, b)) <$> (unit <|> f)
  = (\(_, b) -> b) <$> (unit <|> f)
  by /right unit/ law
  = (\(_, b) -> b) <$> ((),) <$> f
  = (\(_, b) -> b) . ((),) <$> f
  = id <$> f
  = f
```
<span class="qed">QED</span>

_interchange law_, i.e.  `u <*> pure y = pure ($ y) <*> u`
```haskell
 u `monoidalAp` (monoidalPure y)
  = uncurry ($) <$> (u <|> (unit $> y))
  = uncurry ($) <$> (\(x, ) -> (x, y)) <$> (u <|> unit)
  = uncurry ($) . (\(x, _) -> (x, y)) <$> (u <|> unit)
  = (\(x, _) -> x y) <$> (u <|> unit)
  = (\(x, _) -> x y) <$> (,()) <$> u
  = (\(x, _) -> x y) . (,()) <$> u
  = (\(x, _) -> x y) . (,()) <$> (\(_, x) -> x) <$> (unit <|> u)
  = (\(x, _) -> x y) . (,()) . (\(_, x) -> x) <$> (unit <|> u)
  = (\(_, x) -> x y) <$> (unit <|> u)
  = uncurry ($) . (\(_, x) -> ($ y, x)) <$> (unit <|> u)
  = uncurry ($) <$> (\(_, x) -> ($ y, x)) <$> (unit <|> u)
  = uncurry ($) <$> ((unit $> ($ y)) <|> u)
  = uncurry ($) <$> ((monoidalPure ($ y)) <|> u))
  = (monoidalPure ($ y)) `monoidalAp` u
```
<span class="qed">QED</span>

_composition law_ i.e. `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`

```haskell
monoidalPure (.) `monoidalAp` u `monoidalAp` v `monoidalAp` w
  = (uncurry ($) <$> ((unit $> (.)) <|> u)) `monoidalAp` v `monoidalAp` w
  = uncurry ($)
    <$> ((uncurry ($)
           <$> ((unit $> (.)) <|> u)) <|> v)
    `monoidalAp` w
  = uncurry ($)
    <$> (\((x,y),z) -> (x y, z))
    <$> ((unit $> (.)) <|> u <|> v)
    `monoidalAp` w
  = uncurry ($) . (\((x,y),z) -> (x y, z))
    <$> ((unit $> (.)) <|> u <|> v)
    `monoidalAp` w
  = (\((x,y),z) -> x y z)
    <$> ((unit $> (.)) <|> u <|> v)
    `monoidalAp` w
  = (\(x,y) -> x . y)
    <$> (u <|> v)
    `monoidalAp` w
  = uncurry (.) <$> (u <|> v) `monoidalAp w
  = uncurry ($)
    <$> (uncurry (.) <$> (u <|> v)) <|> w
  = uncurry ($) . (\((x,y),z) -> (x . y, z))
    <$> (u <|> v <|> w)
  = (\((x,y),z) -> x . y $ z)
    <$> (u <|> v <|> w)
  = (\((x,y),z) -> x (y z))
    <$> (u <|> v <|> w)
  = uncurry (.)
    <$> (u <|> v <|> w)
```
And from the other side:
```haskell
u `monoidalAp` (v `monoidalAp` w)
  = uncurry ($) <$> (u <|> (v `monoidalAp` w))
  = uncurry ($) <$> (u <|> (uncurry ($) <$> (u <|> w)))
  = uncurry ($) <$> (\((x,y) -> (x, uncurry ($) y)))
    <$> (u <|> (u <|> w))
  = uncurry ($) . (\((x,y) -> (x, uncurry ($) y))
    <$> (u <|> (v <|> w))
  = (\((x, y) -> x (uncurry ($) y)))
    <$> (u <|> (v <|> w))
  = (\(x, (y, z) -> x (y z))
    <$> (u <|> (v <|> w))
  = (\(x, (y, z) -> x (y z)) . (\((x,y),z) -> (x,(y,z)))
    <$> (u <|> v <|> w)
  = (\((x,y),z) -> x (y z))
    <$> (u <|> v <|> w)
  = uncurry (.)
    <$> (u <|> v <|> w)
```
<span class="qed">QED</span>

Equivalence between Applicative and Monoidal functors
=====================================================

From applicative functor to monoidal and back
---------------------------------------------

In this section we consider an applicative functor `f` and we consider tha
applicative `monoidalAp` and `monoidalUnit` obtained from the associated
monoidal functor.  We show that these are equal to what we start with `<*>`
and `pure` of `f`.

The strategy of the proof transform all `<$>` into `<*>` using `f <$> x = pure
f <*> x` and mobe all brackets to the left using the composition law of
applicative functors.

When we'll get to the above canonical form (brackets grouped to the left)
this will be all based on:

<div class="lemma">
**Lemma 1**
```haskell
 (.) (uncurry ($)) . (,) = ($)
```
</div>

**Proof**
```haskell
  ((.) (uncurry ($)) . (,)) f a =
    by definition of (.)
    = (\x -> (.) (uncurry ($)) (x,)) f a
    = (\x -> (uncurry ($)) . (x,)) f a
    by definiiton of (.)
    = (\x y -> (uncurry ($)) (x, y)) f a
    by eta reduction
    = uncurry ($) (f, a)
    = ($) f a
    = f a
```
<span class="qed">QED</span>

Below we show that if we take `monoidalAp` defined via `<|>` which is defined
by `monoidalAp` (which we denote by `<*>` to make all the expressions
shorter), then we will recover our original applicative `<*>` (e.g.
`monoidalAp`):

```haskell
monoidalAp fab fa
  -- by definition of `monoidalAp`
  = uncurry ($) <$> (fab <|> fa)
  -- by defintion of `<|>` 
  = uncurry ($) <$> ((,) <$> fab <*> fa)
  = uncurry ($) <$> (pure (,) <*> fab <*> fa)
  = pure (uncurry ($)) <*> ((pure (,) <*> fab) <*> fa)
  by composition
  = pure (.) <*> pure (uncurry ($)) <*> (pure (,) <*> fab) <*> fa
  = pure ((.) (uncurry ($))) <*> (pure (,) <*> fab) <*> fa
  by lemma 2
  = fab <*> fb
```

<div class="lemma">
  **Lemma 2**

  ```
  pure ((.) (uncurry ($))) <*> (pure (,) <*> fab) = fab
  ```
</div>
**Proof**
```haskell
pure ((.) (uncurry ($))) <*> (pure (,) <*> fab)
  by /composition law/ of applicative functors
  = pure (.) <*> pure ((.) (uncurry ($))) <*> pure (,) <*> fab
  = pure ((.) ((.) (uncurry ($)))) <*> pure (,) <*> fab
  = pure ((.) ((.) (uncurry ($))) (,)) <*> fab
  = pure (((.) (uncurry ($))) . (,)) <*> ab
  by lemma 1
  = pure ($) <*> fab
  since ($) = (id :: (a -> b) -> (a -> b)
  and (pure id) <*> x = x
  = fab
```
<span class="qed">QED</span>

From monoidal to applicative and back
-------------------------------------

In this section we consider a monoidal functor `f`, and then we consisder tha
monoidal functor obtained from the associated applicative functor by means of
`monoidalAp` and `monoidalUnit`.  We prove that what we end with is the
initial monoidal functor `f`.  We use `<|>` and `unit` to denote the initial
monoidal structer of `f`, `<*>` and `pure` is the associated applicative
instance, and we will show that `monoidal` is equal to `<|>`

```haskell
monoidal fa fb
  = (,) <$> fa <*> fb
  = ((,) <$> fa) <|> fb)
  = uncurry ($) <$> ((,) <$> fa) <|> fb
  = uncurry ($) <$> (fmap (\(a, b) -> ((a,),b)) <$> (fa <|> fb))
  = uncurry ($) . (\(a, b) -> ((a,),b) <$> (fa <|> fb)
  = id <$> (fa <|> fb)
  = fa <|> fb
```

```haskell
monoidalUnit
  = pure ()
  -- by definition of pure for the associated applicative functor
  = monoidalPure ()
  = unit $> ()
  = unit
```

References
==========

* [Monoidal Categories in
  ncatlab.org](https://ncatlab.org/nlab/show/monoidal+category)

* [Applicative Programming with
  Effects](https://www.staff.city.ac.uk/~ross/papers/Applicative.html), Conor
  McBride and Ross Peterson, Journal of Functional Programming, 2008.

  There is a final section which briefly mentions equivalnce between strict
  lax monoidal functors and applicative ones (without all the details we went
  through here).  It touches some subtle difference between categorical
  formulation and a higher order functional perspective (also used here),
  which are byond this blog post.
