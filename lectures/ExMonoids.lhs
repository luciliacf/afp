---
title: Exercise: Monoid and Foldable
date: Sep 27, 2017
---

[monoids]: Monoids.html
[exmonoidsLHS]: ExMonoids.lhs
[exmonoids-sol]: ExMonoids-sol.html

Note: You should download the [lhs version][exmonoidsLHS] of this exercise and replace all parts marked undefined.

A solution to this exercise will eventually be available [here][exmonoids-sol]

> {-# LANGUAGE DeriveFunctor #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# OPTIONS_GHC -fdefer-type-errors #-}

> module MonoidFoldable where

> import Prelude hiding (all,any, and, or)
> import Data.Foldable hiding (and, or, any, all)
> import Data.Monoid hiding (All, getAll, Any, getAny)
> import Test.HUnit


Monoids
--------

In the [Lecture about monoids][monoids], we defined the following function that tailors a fold
operation to a specific instance of the `Monoid` class.

> mconcat :: Monoid b => [b] -> b
> mconcat = foldr mappend mempty

For example, because the `String` type is an instance of this class
(using `++` for `mappend`) we can `reduce` a list of `String`s to
a single string.

> tm0 :: Test
> tm0 = reduce ["C", "I", "S", "5", "5", "2" ] ~?= "CIS552"

Numbers can instantiate this class in multiple ways.  Like numbers,
`Booleans` can be made an instance of the `Monoid` class in two different ways.

> newtype All = All { getAll :: Bool }
> newtype Any = Any { getAny :: Bool }

Make sure that you understand these type definitions. We are defining a type
`All` with single data constructor (also called `All`). The argument of this
data constructor is a record with a single field, called `getAll`. What this
means is that `All` and `getAll` allow us to convert `Bool`s to `All` and
back.

~~~~~{.haskell}
ghci> :t All
All :: Bool -> All
ghci> :t getAll
getAll :: All -> Bool
~~~~~~~~~~

Above, `newtype` is like data, but is restricted to a single variant. It is
typically used to create a new name for an existing type. This new name allows
us to have multiple instances for the same type (as below) or to provide type
abstraction.

Your job is to complete these instances that can tell us whether `any` of the
booleans in a list are true, or whether `all` of the booleans in a list are
true. (See two test cases below for an example of the behavior.)

> instance Semigroup All where
>    (<>) = undefined    
>
> instance Monoid All where
>   mempty  = undefined
>
> instance Semigroup Any where 
>    (<>)  =  undefined
>
> instance Monoid Any where
>   mempty  = undefined 

> tm1 :: Test
> tm1 = getAny (mconcat (map Any [True, False, True])) ~?= True

> tm2 :: Test
> tm2 = getAll (mconcat (map All [True, False, True])) ~?= False


Foldable
--------

We can use your `Monoid` instances for `Any` and `All` to generalize
operations to any data structure.

For example, we can generalize the `and` operation to any `Foldable` data
structure using `foldMap`.

> and :: Foldable t => t Bool -> Bool
> and = getAll . foldMap All

Your job is to define these three related operations

> or :: Foldable t => t Bool -> Bool
> or =  undefined 

> all :: Foldable t => (a -> Bool) -> t a -> Bool
> all f = undefined

> any :: Foldable t => (a -> Bool) -> t a -> Bool
> any f = undefined

so that the following tests pass

> tf0 :: Test
> tf0 = or [True, False] ~?= True

> tf1 :: Test
> tf1 = all (>0) [1::Int,2,4,18] ~?= True

> tf2 :: Test
> tf2 = all (>0) [1::Int,-2,4,18] ~?= False

> tf3 :: Test
> tf3 = any (>0) [1::Int,2,4,18] ~?= True

> tf4 :: Test
> tf4 = any (>0) [-1::Int,-2,-4,-18] ~?= False


Application
-----------

Recall our familiar `Tree` type. Haskell can derive the `Functor` instance for
this type so we ask it to do so.

> data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Functor)

And here is an example `Tree`.

> t1 :: Tree String
> t1 = Branch "d" (Branch "b" (l "a" ) (l "c")) (Branch "f" (l "e") (l "g")) where
>        l x = Branch x Empty Empty

Complete the definition of `Foldable` instance for type `Tree` using `foldMap`.

> instance Foldable Tree where
>   foldMap = undefined
 

With this instance, we can for example, verify that all of the sample strings
above have length 1.

> tt1 :: Test
> tt1 = all ((== 1) . length) t1 ~?= True

Finally, look at the documentation for the
[Foldable](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Foldable.html)
class and find some other tree operations that we get automatically for
free.


Oblig-main
----------

> main = runTestTT $ TestList [tm0, tm1, tm2, tf0, tf1,tf2,tf3,tf4, tt1]