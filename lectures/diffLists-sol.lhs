---
title: Exercise: Difference lists
date: September 20, 2017
---

In this problem, you will use first-class functions to implement an alternative
version of lists, called `DList`s, short for *difference lists*.

> module DList where
> import Test.HUnit

Motivation
----------

Difference Lists support O(1) append operations on lists, making them very
useful for append-heavy uses, such as logging, pretty printing and traversing
tree-like data structures in linear time.

See the micro-benchmark section below for experiments you can do once you
have completed the implementation.

Implementation
--------------

The key idea is that we will represent a list using a *function*. This
function should, when given another list, returns the contents of the
difference list prepended to the given list.  You can think of a difference
list as a data structure where we have "factored out" the end of the list.

For example, we might write a regular list like this:

> list :: [Int]
> list = 1 : 2 : 3 : []  -- end is nil

The analogous difference list replaces the nil at the end of the list with
a parameter.

> dlist :: DList Int
> dlist = DList $ \x -> 1 : 2 : 3 : x -- ends with "x"

This parameterization gives us flexibility. We can always fill in the
parameter with `[]` and get a normal list. However, we can also fill in the
parameter with another list, effectively appending [1, 2, 3] to the beginning
of that other list.

The general type definition records that difference lists are represented by
first-class functions.

> data DList a = DList { fromDList :: [a] -> [a] }

These are the \"constructors\" of the data structure; the functions that
we can use to create difference lists.

> empty :: DList a
> empty = DList id
> 

> singleton :: a -> DList a
> singleton x = DList $ \ t -> (x:t)
> 

> append :: DList a -> DList a -> DList a
> append (DList x) (DList y) = DList (x . y)
> 

> cons :: a -> DList a -> DList a
> cons x (DList y) = DList $ \t -> x:(y t)
> 

Once we have constructed a `DList` the *only* way to observe it is to
convert it to a list. This data structure does not support any other form of
pattern matching.

> toList :: DList a -> [a]
> toList x = fromDList x []

And that\'s it.  You\'re on your own for testing here. You should ensure that
`DList`s behave like normal lists.  Add some tests here to ensure that your
implementation is correct.

> 
> testDL :: IO Counts
> testDL = runTestTT $ TestList
>       [ toList empty ~?= ([] :: [Int]),
>         toList (singleton 1) ~?= [1],
>         toList (append (singleton 1) (singleton 2)) ~?= [1] ++ [2],
>         toList (cons 1 empty) ~?= [1] ]
> 


Micro-benchmarks
----------------

If you\'d like to see the difference between using `(++)` with regular lists and
`append` using DLists, in GHCi you can type

~~~~~{.haskell}
*Main> :set +s
~~~~~~~~~~~~

That will cause GHCi to give you timing information for each evaluation that
you do. Then, after you complete this file, you can test out these logging
micro-benchmarks:

> micro1 :: Char
> micro1 = last (t 10000 "") where
>   t 0 l = l
>   t n l = t (n-1) (l ++ "s")

~~~~~{.haskell}
*Main> micro1
's'
(2.80 secs, 4,300,584,976 bytes)
~~~~~~~~~~~~~~

> micro2 :: Char
> micro2 = last (fromDList (t 10000 empty) "") where
>    t 0 l = l
>    t n l = t (n-1) (l `append` singleton 's')

~~~~~{.haskell}
*Main> micro2
's'
(0.02 secs, 10,359,248 bytes)
~~~~~~~~~~~~~

We can also see the effect of using difference list for in-order tree
traversals. (This is not the only way of doing a tree traversal in linear
time.)

> data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show

Here\'s a big, left-biased tree.  The `seq` instruction instructs GHC to
eagerly evaluate it. We don\'t want our benchmark to include time for
constructing this tree \-\- we only want to time the traversal.

> bigLeftTree :: Tree Int
> bigLeftTree = t `seq` t where
>    t     = gen 1000
>    gen 0 = Empty
>    gen n = Branch n (gen (n-1)) Empty

Here is the inorder traversal written naively (as in the lecture notes)
and with difference lists. The only difference between these definitions
is that `[]` is replaced by `empty` and `++` is replaced by `append`.

> infixOrder1 :: Tree a -> [ a ]
> infixOrder1 Empty = []
> infixOrder1 (Branch x l r) = infixOrder1 l ++ [x] ++ infixOrder1 r

> infixOrder2 :: Tree a -> [ a ]
> infixOrder2 = toList . go where
>    go Empty = empty
>    go (Branch x l r) = go l `append` singleton x `append` go r


> micro3 :: Int
> micro3 = last (infixOrder1 bigLeftTree)


~~~~~{.haskell}
*Main> micro3
1000
(0.03 secs, 28,964,080 bytes)
~~~~~~~~~~~

> micro4 :: Int
> micro4 = last (infixOrder2 bigLeftTree)

~~~~~{.haskell}
*Main>  micro4
1000
(0.00 secs, 925,824 bytes)
~~~~~~~~~~~~~~