---
title: Sorted Lists
date: September 2018
---

[SL]: ../homework/SortedList.hs
[hw06-sol]: ../code/hw06.zip
[cwebpage]: 

Edit the file [SortedList.hs][SL] for this problem.
A solution to this homework will eventually be available [here][hw06-sol]

> {-# OPTIONS  -fwarn-tabs -fwarn-incomplete-patterns -fno-warn-type-defaults #-}
> {-# LANGUAGE ScopedTypeVariables #-}

We\'re going to define an abstract type of sorted lists. A `SortedList` a is just an ordinary list
of elements of type `a`, but ordered according to the ordering on `a`. In order to prevent users from
constructing values of this type which violate this invariant, we\'re defining the type and its
operations in a separate module (this one) and only exposing to other importing modules those functions
which respect the invariants of the type.

Abstract types in Haskell
--------------------------

The identifiers after the module name below define exactly what is exported by this module.

> module SortedList ( SortedList,    -- the abstract type (and its instances)
>                     singleton,     -- other functions
>                     toNormalList,
>                     minimum,
>                     numDistinct,
>                     count
>                   ) where
> import Test.HUnit
> import Prelude hiding ( minimum, maxmimum )
> import qualified Data.List as List
> import Data.Coerce
>
> import Data.Monoid

We will define the `SortedList` type (see below), but not export the data constructor
(also called `SortedList`) for this type. What that means is that within this module, we can use
the data constructor to make any `SortedList` we want, even a bad one. But outside this module,
we ensure that users can construct only sorted `SortedList`s by only providing functions that
construct sorted lists. For example, because the one-element list is always sorted, we can safely
expose the singleton function below.

It\'s also safe to extract the wrapped list from a `SortedList` because this doesn\'t affect the original list.

These two functions alone are not very useful, though. In order to do interesting things with sorted lists,
we need to be able to combine them with each other to build sorted lists larger than one element.
It would also be useful to define how to build a sorted list with zero elements.

There is a particular structure for what we\'ve just described above, and it\'s a structure that\'s
ubiquitous in Haskell programming: the `Monoid`. The typeclass `Monoid` may be the first example of
a typeclass you\'ve seen which does not easily align to overloading interfaces defined in your (previous)
favorite programming language. We're going to use the `Monoid` typeclass to define the remainder of our
interface to sorted lists.


Sorted lists
-------------

Like the above, we need to define our abstract type as a wrapper around ordinary lists.
For this, we use Haskell\'s `newtype` keyword, which creates a new type much like `data`, but with the
guarantee that our access to the wrapped type will be with zero runtime overhead.
The `toNormalList` function is just the record selector from this type definition.

> newtype SortedList a = SortedList [a] deriving (Eq, Show)
>
> toNormalList :: SortedList a -> [a]
> toNormalList (SortedList as) = as
>
> singleton :: a -> SortedList a
> singleton a = SortedList [a]

Now, fill in the `Monoid` instance for `SortedLists`.

Hint: keep in mind the properties of sorted lists when writing this instance. This invariant lets
you write faster code than you would otherwise be able to do.

> instance Ord a => Semigroup (SortedList a) where
>    l1 <> l2 = undefined
>
> instance Ord a => Monoid (SortedList a) where
>   mempty = undefined

Make sure that your implementation only produces sorted lists, and also satisfies the properties of
monoids!

> testSortedList :: Test
> testSortedList =
>   let t1 = SortedList [2,4] in
>   let t2 = SortedList [1,5] in
>   let t3 = SortedList [2,3] in
>   TestList [ t1 <> t3 ~?= SortedList [2,2,3,4],    -- <> preserves sorting
>              mempty <> t1 ~?= t1,                  -- left identity
>              t1 <> mempty ~?= t1,                  -- right identity
>              (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
>            ]


Some Other Operations
----------------------

While merely the operations defined above are sufficient to define the analogues of most list functions
for `SortedLists` also, implementing a replica of the list library only in terms of the above abstraction
would necessarily come at a performance cost; it would necessitate conversion to and from the `SortedList`
representation, which requires computational work.

On the other hand, if we were to implement these functions here, we could take advantage of the internal
sorted-ness invariant of the list in order to make certain operations faster. Let\'s do that.

A first example: minimum.

> minimum :: SortedList a -> Maybe a
> minimum = undefined
> testMinimum :: Test
> testMinimum =
>   let t1 = SortedList [1,3,5] in
>   let t2 = SortedList ([] :: [Int]) in
>   let t3 = SortedList [1, error "kaboom!"] <> SortedList[2] in
>   TestList [ minimum t1 ~?= Just 1   -- the minimum of a non-empty sorted list
>            , minimum t2 ~?= Nothing  -- the minimum of an empty sorted list
>            , minimum t3 ~?= Just 1   -- minimum need not examine whole list
>            ]

In the above test cases, you will get an error if your implementation does not take advantage of the
sorted-ness invariant to avoid extra computation.

Another operation which can be made more efficient for `SortedLists` is calculating the number of distinct
values in the list.

> numDistinct :: Ord a => SortedList a -> Int
> numDistinct = undefined
> testNumDistinct :: Test
> testNumDistinct = TestList
>  [numDistinct (SortedList [1::Int,1,3,3,5]) ~?= 3,
>   numDistinct (SortedList ([]::[Int])) ~?= 0]

We can also count how many times every distinct value occurs in the list:

> count :: Eq a => SortedList a -> [(a, Integer)]
> count = undefined

Your implementation of count should result in another genuine, legal `SortedList`. Convince yourself that
it does before moving on, keeping in mind the Ord instances for tuples are left-to-right lexicographic orderings,
dependent on the underlying `Ord` instances of the tuple\'s elements.

> testCount :: Test
> testCount =
>   let xs = SortedList "abbcccdddd" in
>   count xs ~?= [('a', 1),('b',2),('c',3),('d',4)]

At this point, one important typeclass seems to have been left out in our interface to the `SortedLis`t type: `Functor`.
It seems natural that we should be able to map a function over a `SortedList`, just like we can over an ordinary list.
This doesn\'t work, though. Why?

  * Operations on values of type `SortedList a` only make sense for a type `a` that is an instance of `Ord`.
    So, we would like that function `fmap` would have the following type: `fmap :: (ord a, Ord b) :: (a -> b) -> f a -> f b`,
    rather than the type declared in the `Functor` typeclass, namely  `fmap :: (a -> b) -> f a -> f b`. This problem could be
    circumvented by definind an `OrdFunctor` typeclass:
    
         class Ordfunctor where
            fmapOrd :: (Ord a, Ord b) => (a -> b) -> f a -> f b

     But this might get annoying if we had lots of different kinds of Functors, for example `EqFunctor`, `MonoidFunctor` etc.

  * Note also that we should be careful when making `SortedList` a functor; the following is not `fmap`: 
    
         notFmap :: (Ord a, Ord b) => (a -> b) -> SortedList a -> SortedList b
         notFmap f (SortedList xs)     = SortedList (fmap f xs)
         
    Why not? Consider `notFmap negate (SortedList [1,2,3])`. That will produce the `SortedList [-1,-2,-3]`,
    which presumably violates our invariants \–\- it\'s sorted backwards. We should make sure that our
    `fmap` is invariant-preserving.


At this point, we have finished defining the internal implementation of `SortedList`s. Because all the operations we
expose to the user of this module respect the sorted-ness property of `SortedList`s, we know that any value of this
type must be sorted. So, once we go back to the file `Main.lhs`, we will be we are prevented from making \"illegal\"
values of `SortedList`s.