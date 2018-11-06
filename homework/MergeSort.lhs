---
title: Merge Sort
date: September 2018
---

[MS]: ../homework/MergeSort.hs
[hw06-sol]: ../code/hw06.zip
[cwebpage]: 

Edit the file [MergeSort.hs][MS] for this problem.
A solution to this homework will eventually be available [here][hw06-sol]


> module MergeSort where
> import SortedList
> import Data.Monoid
> import Test.HUnit

A warm-up exercise: write the function `insert`, which takes an element and a sorted list and inserts
the element into the list at the first position where it is less than or equal to the next element.
For this definition, do not use any functions from the Data.List library (which, indeed, contains such a function).

> insert :: Ord a => a -> [a] -> [a]
> insert = undefined

Using this function, we can define the *insertion sort* algorithm over lists. Insertion sort, like several
other sorting algorithms, is incremental \-\- it works by processing a single element of the input unsorted
list at a time, and when it finishes processing the input, its work is done.
Using the `insert` function above, write a function which takes a list and returns the list sorted.
You must express your answer in terms of a `fold`.

> insertionSort :: Ord a => [a] -> [a]
> insertionSort = undefined

Keep in mind that this sorting algorithm, although succinct, is not efficient \-\- it has $O(N^2)$ asymptotic complexity.

You may have noticed that the insert function above has an invariant attached to its description; namely,
that its list argument is already sorted. If that invariant holds, then insert guarantees that its output will
also be sorted; otherwise, there is no such guarantee.

A leading question: what if we could keep track of the \"sortedness\" of a list using the type system?
You already know that we can \-\- that\'s precisely what we did in `SortedList.lhs`.

SortedLists to the Rescue
-------------------------

The interface we built for `SortedList`s can be useful. One particularly obvious useful thing to do with `SortedList`s
is to construct them from arbitrary lists \-\- that is, to sort an arbitrary list and produce a `SortedList` with the result.

> sortedFromList :: Ord a => [a] -> SortedList a
> sortedFromList = undefined

By projecting out the underlying list, we get a sorting function, that we\'ll call `sortedListSort`.

> sortedListSort :: Ord a => [a] -> [a]
> sortedListSort = toNormalList . sortedFromList
> testSortedFromList :: Test
> testSortedFromList =
>   let unsorted = [51,67,89,95,14,31,28,87,0,25]
>       sorted   = [0,14,25,28,31,51,67,87,89,95] in
>   sortedListSort unsorted ~?= sorted

One thing you may have noticed while writing the above function is that there is only one place you could have made
reference to the `SortedList` type specifically: in the use of the `singleton` operation. Indeed, this operation is
the only `SortedList`-specific way to create new `SortedList`s \-\- any other way comes through the `Monoid` instance.
Perhaps there\'s some common pattern here that we could abstract! (There is.) Let\'s express it by making the singleton
function into a parameter of a new function, that we will call `foldMapList`, so that we can rewrite the algorithm
above like so:

> sortedFromList' :: Ord a => [a] -> SortedList a
> sortedFromList' = foldMapList singleton

Again, we can project out the underlying list to get a list sorting function.

> sortedListSort' :: Ord a => [a] -> [a]
> sortedListSort' = toNormalList . sortedFromList'
> testSortedFromList' :: Test
> testSortedFromList' =
>   let unsorted = [47,80,28,47,45,76,1,35,19,1] in
>   sortedListSort' unsorted ~?= sortedListSort unsorted  -- old & new agree

In order to make this work, you need to define the `foldMapList` combinator.

> foldMapList :: Monoid m => (a -> m) -> [a] -> m
> foldMapList f = undefined

The type of `foldMapList` is very general \-\- we can use this function to combine arbitrary lists by providing a
function that maps their contents to some particular `Monoid` (such as `SortedList`). For instance, `foldMapList Sum`
gives us the `sum` of the numbers in the list; `foldMap` Product gives us their product.

A small exercise: using `foldMapList` and the `Sum` and `Product` newtypes you learned about in `SortedList.lhs`,
implement the following function, which takes a doubly-nested list of numbers as input, and returns the sum of the
product of each of the inner lists. In your solution, do not explicitly use any of the ordinary numeric operations
like `(+)`, `(*)`, `sum`, or `product`, and eschew explicit recursion.

> sumOfProducts :: Num a => [[a]] -> a
> sumOfProducts = undefined
>
> testSumOfProducts :: Test
> testSumOfProducts = sumOfProducts [[1],[2,3],[4,5,6],[7,8,9,10]] ~?= 5167

Like Merge Sort, the `sortedListSort` function is based on merging sorted lists together.
This merge-sort-like algorithm has a flaw, though: it\'s quadratic in runtime. Why?

For any singleton `SortedList [a]` and any other `SortedList as`, computing `SortedList [a] <> SortedList as` is
identical not only in resultant value, but also in algorithmic structure to computing the result of `insert a as`.
The definition of `foldMapList` linearly scans across its input list, successively combining values using `(<>)` \-\-
and so, like insertion sort, the whole whole algorithm ends up executing a quadratic number of comparisons.

A real merge sort algorithm, as you likely know, divides its input more intelligently than the one we\'ve written
above in terms of `foldMapList`. By dividing its input roughly in half every iteration, it only has to do a
logarithmic amount of merging.

To make our merge sort do this, we need to use a different kind of `foldMap`!

The Foldable Typeclass
======================

Function `foldMapList` can itself be even further generalized. We already know that lists are not the only data
structures which support folding \-\- we\'ve seen folds for trees of various kinds and for other data structures
as well. As a result, it makes sense to allow some kind of `foldMap` operation for those structures also.
In the standard library, we therefore have:

~~~~~{.haskell}
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
~~~~~~~~~~~~

That is to say, `foldMap` is a method of yet another type class, `Foldable`, of which the type constructor `[]`
is an instance. Implementing this interface roughly corresponds to saying, \"this data structure contains some
elements, and I know how to do a fold across them\". To implement the `Foldable` class for some type, we
just need to implement `foldMap`.

Implement the `Functor` and `Foldable` instances for the `Crispy` datatype. Remember to keep in mind a
guiding principle: when you are confused, don\'t think about what things are supposed to mean;
just follow the types and see where they take you.

> data Crispy a = Snap a [a] a
>               | Crackle [[Crispy a]]
>               | Pop Integer deriving (Eq,Show)
>
> instance Functor Crispy where
>   fmap = undefined
>
> instance Foldable Crispy where
>   foldMap = undefined
>
> testCrispy :: Test
> testCrispy =
>   let c1, c2, c3, c4, c5 :: Crispy Integer
>       c1 = fmap (+1) (Snap 0 [1,2,3] 4)
>       c2 = Snap 700 [] 600
>       c3 = Pop 1234567890
>       c4 = Crackle [[c3, c1], [c3, c1]]
>       c5 = fmap (subtract 1) (Crackle [[c1, c2], [c1, c3]]) in
>   TestList [ 15 ~?= getSum (foldMap Sum c1)
>            , 1 ~?= getProduct (foldMap Product c3)
>            , "0123469959901234" ~?= foldMap show c5]

Back to Sorting
=================

In order to express an efficient merge sort in terms of `foldMap`, we need to design a data structure
that represents a sequence of elements (just like a list), but whose `Foldable` instance uses a
divide-and-conquer strategy, rather than the `[]` instance\'s linear `fold` pattern of recursion.

> newtype DivideList a = DivideList { getDivideList :: [a] } deriving (Eq, Show)

A `DivideList` is a `Monoid`. Implement its `Monoid` instance below.

> instance Semigroup (DivideList a) where
>   (<>) = undefined
>
> instance Monoid (DivideList a) where
>   mempty = undefined

And of course, the vital part: we need `DivideList`s to be `Foldable`, but in a different way.
First, implement the `divide` function, which splits a `DivideList` in its middle, returning
the result of the split.

> divide :: DivideList a -> (DivideList a, DivideList a)
> divide = undefined
>
> testDivide :: Test
> testDivide = TestList [ divide (DivideList "abcd") ~?=
>                        (DivideList "ab", DivideList "cd"),
>                       divide (DivideList "abcde") ~?=
>                        (DivideList "ab", DivideList "cde"),
>                       divide (DivideList "") ~?=
>                        (DivideList "", DivideList "") ]

Using this function, we can define the `Foldable` instance for `DivideList`s. Note that this definition
is trickier than it seems. If you encounter an infinite loop, it means that you have not covered one of
a particular set of slightly non-trivial edge cases.

> instance Foldable DivideList where
>   foldMap f xs =
>     case divide xs of
>       (DivideList as, DivideList bs) -> undefined
>
> testDivideList :: Test
> testDivideList =
>   let xs = DivideList [1,2,3]
>       ys = DivideList [] in
>   TestList [ Product 6 ~?= foldMap Product xs
>            , Sum 0     ~?= foldMap Sum ys
>            ]

Now that we know how general the `foldMap` function is, have a look at the implementation of `sortedListSort`
above \-\- does its input type need to only be a list? Generalize its type signature so that it outputs a list of
sorted elements located inside an arbitrary `Foldable` structure.

> -- foldSort ::
> foldSort = undefined -- implementation should use foldMap

By parameterizing over any `Foldable` container, what we\'ve done is to factor out the folding strategy
into the choice of original container! To pick a different divide-and-conquer strategy, we need only
specify a different container type, and give it a `Foldable` instance that folds along different creases.

So, while our `sortedListSort` was $O(N^2)$, we can produce a differently structured algorithm by instead
folding over a DivideList instead:

> realMergeSort :: Ord a => [a] -> [a]
> realMergeSort = foldSort . DivideList

If you've done everything correctly, this main function should return rather quickly.

> main :: IO ()
> main = (print . last . realMergeSort) [100000,99999..0]

Concluding Thoughts About This Exercise
----------------------------------------

The important takeaway here is this: `foldMap` defines once and for all a universal \"divide-and-conquer\"
algorithm \-\- all we need to do to use it is to provide a way to \"divide\" an input container
(i.e\. give a `Foldable` instance), then give a way to compute on those elements (i.e\. the mapped function `a -> m`)
and a way to combine (\"conquer\") the results of that computation (i.e\. a `Monoid` instance for the result type).

Almost any divide-and-conquer algorithm can be fit into this framework, and that means we can avoid
repeating ourselves when writing such programs. We can reuse the division strategy of `DivideList` when writing
some other algorithm, and likewise for the sorted-merging combination strategy of `SortedList`.

