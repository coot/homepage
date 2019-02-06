Why Monadic IO?
===============

Have you every wondered why monads turn out to be the abstraction behind `IO`?
To find an answer we will build a two (very incomplete) models for `IO`:

* one that is very common in imperative languages, based on a sequence of `IO`
  actions
* a recursive one, that turns out to be monadic.

> {-# LANGUAGE GADTs #-}
> module MonadicIO where

> import           Control.Monad (ap)
> import           Data.List.NonEmpty (NonEmpty (..))
> import qualified Data.List.NonEmpty as NE
> import qualified System.IO as IO

Monoidal IO
-----------

> data IOAction x
>   = Write  FilePath String
>   | Read   FilePath
>   | Return x


Mostly found in imperative languages, where IO is a sequence of operations.
And where `Return` has the semantics of a final statement.  There are many
algebras that provide a sequences.  The most general one we could pick are non
associative semigroups (also called
[magmas](https://en.wikipedia.org/wiki/Magma_%28algebra%29)).  It would have two problems:

* since it's non associative we could interpret sequences depending on the
  bracketing, but this is too much freedom for us.  We want all expression
  build by putting brackets differently be always have the same semantics
* it's not strictly necessary but, having a unit for the binary operation
  might be convienient

For that reason, we'd like to use associative unital magmas, e.g. a monoid.
The good choice should be the most general such object, i.e. a free monoid

> type MonoidalIO x = [IOAction x]

For a side note: `DList` is a free monoid, while `[]` is free in the class
of left strict monoids, e.g. monoids satisfying: `undefined <> == undefined`.

Let us provide a way to actually run `MonoidalIO`, since we are in *Haskell*
let us interpret `MonoidalIO` in the `IO` monad.

> runMonoidalIO :: MonoidalIO x -> IO x
> runMonoidalIO (Return x : _)            = return x
> runMonoidalIO ((Write path str) : next) =
>   IO.writeFile path str >> runMonoidalIO next
> runMonoidalIO ((Read path) : next)      =
>   IO.readFile path >> runMonoidalIO next

Monadic IO
----------

There is yet another way of organising a sequence of computations.  And it is
especially compelling in a language with algebraic data types.

In a recursive style we can describe the whole program progression using
a single recursive data structure, where each computation carries
a continuation.

> data MonadicIO x
>   = WriteM FilePath String (MonadicIO x)
>   | ReadM  FilePath (String -> MonadicIO x)
>   | ReturnM x

> instance Functor MonadicIO where
>   fmap f (ReturnM x)      = ReturnM (f x)
>   fmap f (ReadM path io)  = ReadM path ((fmap . fmap) f io)
>   fmap f (WriteM path str io) = WriteM path str (fmap f io)

We can transform any `MonoidalIO` into `MonadicIO`.

> -- | transform `MonoidalIO` into `MonadicIO`
> --
> fromMonoidalIO :: MonoidalIO x -> MonadicIO x
> fromMonoidalIO ((Read path) : next)      = ReadM path (\_ -> fromMonoidalIO next)
> fromMonoidalIO ((Write path str) : next) = WriteM path str (fromMonoidalIO next)
> fromMonoidalIO (Return x : _)            = ReturnM x

We cannot transform `MonadicIO` to `MonoidalIO`, only because we did not
provide a way to bind data read from a file in `MonoidalIO` just for
simplicity of the presentation.  But the two approaches should be equivalent.

We also need a way to run `MonadicIO`, again since we are in *Haskell* we'll
provide a map to `IO`:

> runMonadicIO :: MonadicIO x -> IO x
> runMonadicIO (ReturnM x)          = return x
> runMonadicIO (ReadM path io)      = IO.readFile path >>= runMonadicIO . io
> runMonadicIO (WriteM path str io) = IO.writeFile path str >> runMonadicIO io

But this allows only to run expressions of type `MonadicIO x`, we still need
a way to run expressions of type `MonadicIO (MonadicIO (... x))`.  Thus we
need:

> joinMonadicIO :: MonadicIO (MonadicIO x) -> MonadicIO x
> joinMonadicIO (ReturnM io)       = io
> joinMonadicIO (WriteM fp str io) = WriteM fp str (joinMonadicIO io)
> joinMonadicIO (ReadM path io)    = ReadM path (joinMonadicIO . io)

In `MonoidalIO` we relied on associativity of the list concatenation,
a similar requirements is needed here.  If we have an expression of type `x ::
MonadicIO (MonadicIO (MonadicIO x))` there are two ways of running it, by
using of the two maps:

> assoc1 :: MonadicIO (MonadicIO (MonadicIO x)) -> MonadicIO x
> assoc1 = joinMonadicIO . joinMonadicIO

or

> assoc2 :: MonadicIO (MonadicIO (MonadicIO x)) -> MonadicIO x
> assoc2 = joinMonadicIO . fmap joinMonadicIO

We really want both `assoc1` and `assoc2` to be equal.  This way the way that
we build an expression of type `MonadicIO x` does not matter.  This is exactly
the associativity law for monads.  And indeed `MonadicIO` is a monad, and
`joinMonadicIO` is its `join` operator.  This is in analogy to the
associativity law of monoids in `MonoidalIO`.

> instance Applicative MonadicIO where
>    pure  = ReturnM
>    (<*>) = ap
>
> instance Monad MonadicIO where
>    return = ReturnM
>    ReturnM x >>= f          = f x
>    WriteM path str io >>= f = WriteM path str (io >>= f)
>    ReadM path io >>= f      = ReadM path (fmap (>>= f) io)

Let me note, that Haskell `IO` monad is build differently though, to give much
more flexibility for building `IO` actions for many different operations
supported by OS.  In the recursive style we need to built-in all the
operations that are possible to run.  This would be too restrictive for
a general purpose language.  Haskell abstracts over a state monad, e.g. a type
`s -> (s, a)` (where `s` is a [state of the
world](https://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Types.html#v:IO)),
but it is still a monad, and monad laws guarantee that the semantic of an
expression is independent of bracketing.  It is also a recursive type, though
the recursiveness is hidden in the monadic `join` and `>>=` operators.

Conclusions
-----------

Using a recursive `IO` we end up with a type that satisfies monad laws.  The
monad associativity guarantees that when we build the expression using `do`
notation (e.g. `>>=` or `join`) the bracketing will not change the semantics
of an expression.

At last let stress some benefits of recursive/monadic `IO`:

* much easier support for actions the return values, e.g. in `MonoidalIO` we
  did not have access to data read from a file.  In a functional language,
  recursive / monadic IO does not require any thing more than lambdas to bind
  the return value.
* values returned by `IO` operations are trapped inside the `MonoidalIO`
  monad.  This gives a clear indication which functions have access to IO and
  which are pure.
* For a lasy language using  a recursive data type is a compelling choice.  In
  Haskell, the evaluation is guided by pattern matching, every bind (as
  you can consult above), evaluates just a single layer of a computation.
