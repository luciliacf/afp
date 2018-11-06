---
title: Exercise: General Monadic Functions
date: Oct 25, 2017
---

[Exmonadslhs]: MonadicFunctions.lhs
[Exmonads-sol]: MonadicFunctions.html

*Note:* You may download the [lhs version][Exmonadslhs]
of this module and replace all parts marked undefined. Eventually, the [complete version][Exmonads-sol] will
be made available.


> module MonadExercise where

> import Prelude hiding (mapM, foldM, sequence)
> import Test.HUnit


Generic Monad Operations
========================

This problem asks you to recreate some of the operations in the
[Control.Monad](https://hackage.haskell.org/package/base-4.9.1.0/docs/Control-Monad.html)
library. You should *not* use any of the functions defined in that library to
solve this problem.  (These functions also appear in more general forms
elsewhere, so other libraries that are off limits for this problem include
`Control.Applicative`, `Data.Traversable` and `Data.Foldable`.)

Add tests for each of these functions with at least two test cases, one using
the `Maybe` monad, and one using the `List` monad.


> -- (a) Define a monadic generalization of map

> mapM :: Monad m => (a -> m b) -> [a] -> m [b]
> mapM = error "mapM: unimplemented"

> testMapM :: Test
> testMapM = undefined

> -- (b) Define a monadic generalization of foldl

> foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
> foldM = error "foldM: unimplemented"


> testFoldM :: Test
> testFoldM = undefined

> -- (c) Define a generalization of monadic sequencing that evaluates
> -- each action in a list from left to right, collecting the results
> -- in a list.

> sequence :: Monad m => [m a] -> m [a]
> sequence = error "sequence: unimplemented"

> testSequence :: Test
> testSequence = undefined

> -- (d) Define the Kleisli "fish operator", a variant of composition

> (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
> (>=>) = error ">=>: unimplemented"

> testKleisli :: Test
> testKleisli = undefined

For more information about this operator, see the explanation at the bottom of
[this page](http://www.haskell.org/haskellwiki/Monad_laws).

> -- (e) Define the 'join' operator, which removes one level of
> -- monadic structure.

> join :: (Monad m) => m (m a) -> m a
> join = error "join: unimplemented"

> testJoin :: Test
> testJoin = undefined

> -- (f) Define the 'liftM' function

Define `liftM`, which promotes functions `a -> b` to functions over
actions in a monad `m a -> m b`.

> liftM   :: (Monad m) => (a -> b) -> m a -> m b
> liftM = error "liftM: unimplemented"

> testLiftM :: Test
> testLiftM = undefined

Thought question: Is the type of `liftM` similar to that of another
function we\'ve discussed recently?

> -- (g) And its two-argument version ...

Now define a variation of `liftM`, `liftM2`, that works for
functions taking two arguments:

> liftM2  :: (Monad m) => (a -> b -> r) -> m a -> m b -> m r
> liftM2 = error "liftM2: unimplemented"

> testLiftM2 :: Test
> testLiftM2 = undefined


> -- (h) Define the `ap` function

Define `ap`, which applies a monadic action to a monadic value, returning a monadic action.

> ap  :: (Monad m) => m (a -> b) -> m a -> m b
> ap  = undefined

> testAp :: Test
> testAp = TestList [ [(*2), (^2)] `ap` [3,4] ~?= [6,8,9,16],
>                     [drop 2, take 2] `ap` [[1,2,3],[4,5],[6,6,6]] ~?= [[3],[],[6],[1,2],[4,5],[6,6]] ]

General Applicative Functions
=============================

Which of these functions above can you equivalently rewrite using `Applicative`?
i.e\. for which of the definitions below, can you replace `undefined` with
a definition that *only* uses members of the `Applicative` type class.
(Again, do not use functions from `Control.Applicative`, `Data.Foldable` or
`Data.Traversable` in your solution.)

If you provide a definition, you should write test cases that demonstrate that it
has the same behavior on `List` and `Maybe` as the monadic versions above.

> -- NOTE: you may not be able to define all of these, but be sure to test the
> -- ones that you do

> mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
> mapA f xs = undefined

> foldA :: Applicative f => (a -> b -> f a) -> a -> [b] -> f a
> foldA = undefined

> sequenceA :: Applicative f => [f a] -> f [a]
> sequenceA = undefined

> kleisliA :: Applicative f => (a -> f b) -> (b -> f c) -> a -> f c
> kleisliA = undefined

> joinA :: (Applicative f) => f (f a) -> f a
> joinA = undefined

> liftA   :: (Applicative f) => (a -> b) -> f a -> f b
> liftA f x = undefined

> liftA2  :: (Applicative f) => (a -> b -> r) -> f a -> f b -> f r
> liftA2 f x y = undefined
