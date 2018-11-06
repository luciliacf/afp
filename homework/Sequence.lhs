---
title: Sequences using Balanced Trees
author: Lucilia Figueiredo
---

[seqhs]: Sequence.hs
[seq-sol]: ../code/Sequence.zip



Download the plain-Haskell version of the file [Sequence][seqhs] and answer each question,
filling in code as appropriate. A solution to this problem will eventually be available [here][seq-sol]

> {-# OPTIONS_GHC -fdefer-type-errors #-}
> {-# LANGUAGE MonadComprehensions, ScopedTypeVariables #-}

In this problem, you will reuse the ideas of balanced trees to develop a data structure for appendable,
random-access sequences.

This problem draws together ideas that you have seen on past homework assignments and
extends them with problems about defining functor and monad operations for list-like structures.

> module Sequence where
> import Test.HUnit hiding (State)
> import Test.QuickCheck
> import Test.QuickCheck.Function
> import Control.Applicative(Alternative(..))
> import Control.Monad(ap, liftM, liftM2, guard, forM, foldM)

A sequence is a data structure, much like a list, that supports positional-based indexing of an
ordered collection of elements.

Here is the interface that a sequence should satisfy.

> class (Monad l, Foldable l) => Sequence l where
>    -- construction
>    nil    :: l a
>    single :: a -> l a
>    append :: l a -> l a -> l a
>    -- position based operations
>    first  :: l a -> Maybe a
>    final  :: l a -> Maybe a
>    index  :: Int -> l a -> Maybe a
>    insert :: Int -> a -> l a -> Maybe (l a)

Note that Sequence is a subclass of `Monad` and `Foldable`. Any type that is an instance
of `Sequence` will also have to be an instance of both of these two structures.

Because sequences are monads with the `MonadComprehensions` language extension above,
we can also use the list comprehension notation with sequences, as if they were lists.

> pairs :: Sequence l => l a -> l b -> l (a,b)
> pairs xs ys = [(x,y) | x <- xs, y <- ys ]

For example, a list can implement the sequence interface. Note that in `index` and
`insert` below, if the position is out of range, the result is `Nothing`.

> instance Sequence [] where
>    nil        = []
>    single x     = [x]
>    append       = (++)
>    first l      = guard (not (null l)) >> return (head l)
>    final l      = guard (not (null l)) >> return (last l)
>    index n l    = guard (0 <= n && n < length l) >> return (l !! n)
>    insert n x l = guard (0 <= n && n < length l) >> return (before ++ x : after)
>       where
>        (before, after) = splitAt n l

However, these operations are inefficient for lists. Although first is constant
time, all of the other operations are $O(n)$, where n is the length of the first list argument.

We can do better.

Consider the following AVL-tree inspired data structure. Below, all of the values are
stored at the leaves of the structure (using the `Single` data constructor).
The `Branch` constructor includes the cached height of the tree (so that we can rebalance)
and the cached length of the sequence (so that we can efficiently index).

> data AVL a =
>      Empty
>    | Single a
>    | Branch
>          Int       -- cached number of elements
>          Int       -- cached height
>          (AVL a)   -- left child
>          (AVL a)   -- right child
>        deriving (Show)

For example, here is an example AVL-based sequence, containing the numbers 7, 3, 4, 5 in that order.

> seq1 :: AVL Int
> seq1 = Branch 4 2 (Branch 2 1 (Single 7) (Single 3))
>                   (Branch 2 1 (Single 4) (Single 5))

As part of this problem, you will implement the following functions, as well as
complete an instance of `Foldable` and `Monad` for the AVL type.

> instance Sequence AVL where
>    nil        = Empty
>    single     = Single
>    append     = avlAppend
>    first      = avlFirst
>    final      = avlFinal
>    index      = avlIndex
>    insert     = avlInsert

For example, here is a test case that you should be able to satisfy by the end of the assignment.

> testPairs :: Test
> testPairs = "pairs" ~: toList (pairs seq1 seq1) ~=?
>   [(7,7),(7,3),(7,4),(7,5),(3,7),(3,3),(3,4),(3,5),
>    (4,7),(4,3),(4,4),(4,5),(5,7),(5,3),(5,4),(5,5)]

> -- (a) first and final

AVL trees trade constant time \"head\" access for a $O(lg n)$ running time for all other
operations. Here, accessing either the first or last element may take time $O(lg n)$.

> -- | access the first element of the sequence, if there is one.
> avlFirst :: AVL a -> Maybe a
> avlFirst = error "first: unimplemented"

> -- | access the last element of the list, if there is one (similar to above)
> avlFinal :: AVL a -> Maybe a
> avlFinal = error "avlFinal: unimplemented"

> testFirst :: Test
> testFirst = TestList [ "first" ~: first seq1 ~=? Just 7,
>                        "final" ~: final seq1 ~=? Just 5]

> -- (b) Reducing sequences

The `Foldable` type class allows us to treat sequences like lists when it comes to reducing
them to values. We can make an instance of this class merely by providing a definition of
the `foldr` function; all other operations, such as length are given default definitions in
terms of `foldr`.

> instance Foldable AVL where
>  -- The default definition of the length function looks something like this:
>    length = foldr (\x s -> s +1) 0
>    -- Override this definition with an optimized version that is O(1)
>    -- Finish the `foldr` definition below so that it is O(n) (Hint: see HW2)
>    foldr f b Empty            = b
>    foldr f b (Single x)       = f x b
>    foldr f b (Branch _ _ xs ys) = undefined

Use `foldr` to convert sequences to standard lists.

> toList :: Sequence l => l a -> [a]
> toList = undefined

We use the `toList` function to implement the equality function for this type.
We only care about the sequence of values that appear, not the tree structure.

> instance Eq a => Eq (AVL a) where
>    l1 == l2 = toList l1 == toList l2

> testFoldable :: Test
> testFoldable =
>     TestList [ "length" ~: length seq1 ~?= 4
>              , "toList" ~: toList seq1 ~?= [7,3,4,5]
>              , "sum"    ~: sum    seq1 ~?= 19
>              ]

> -- (c)  Indexing

We use the stored length to navigate the tree structure when we reference an element
in the list by its index. Position 0 is the element at the head of the sequence, counting
up to length-1. If the given index is not in range, this function should return `Nothing`.
It should run in $O(lg n)$ time.

> avlIndex :: Int -> AVL a -> Maybe a
> avlIndex = undefined

> testAvlIndex = TestList [ "index 0"  ~: avlIndex  0 seq1 ~?= Just 7,
>                           "index 1"  ~: avlIndex  1 seq1 ~?= Just 3,
>                           "index 2"  ~: avlIndex  2 seq1 ~?= Just 4,
>                           "index 3"  ~: avlIndex  3 seq1 ~?= Just 5 ]

> -- (d) Insert

Next, adapt the AVL insertion function from your previous homework to enable insertion
into this structure. If you did not successfully complete the AVL assignment, the TAs
will show you the solution during office hours.

To make working with this data structure easier, we create the following \"smart constructor\"
that calculates this cached information for us.

> branch :: AVL a -> AVL a -> AVL a
> branch x y = Branch (length x + length y) (1 + max (height x) (height y)) x y

Accessing the height of the tree is also a constant time operation.

> height :: AVL a -> Int
> height Empty = 0
> height (Single x) = 0
> height (Branch _ k s1 s2) = k

> avlInsert :: Int -> a -> AVL a -> Maybe (AVL a)
> avlInsert = undefined

This test case checks that the value is inserted at the correct position, but not whether the result is balanced.

> testAvlInsert :: Test
> testAvlInsert = TestList [
>     "insert 0 " ~: toList <$> insert 0 1 seq1 ~?= Just [1,7,3,4,5]
>   , "insert 1 " ~: toList <$> insert 1 1 seq1 ~?= Just [7,1,3,4,5]
>   , "insert 2 " ~: toList <$> insert 2 1 seq1 ~?= Just [7,3,1,4,5]
>   , "insert 3 " ~: toList <$> insert 3 1 seq1 ~?= Just [7,3,4,1,5]
>   , "insert 4 " ~: toList <$> insert 4 1 seq1 ~?= Just [7,3,4,5,1]
>   ]

We\'ll make sure that our trees stay balanced with quickcheck.

> -- (e) Testing with quickcheck

Let\'s make some random sequences for testing!

Complete the arbitrary instance, making sure you use the insert function above to construct
some of the AVLs. Note: if you use Branch your generated sequence may not be balanced.
We want to only generate balanced trees.

> instance (Show a, Arbitrary a) => Arbitrary (AVL a) where
>     arbitrary = undefined
>     shrink _  = undefined

Now we can compare the stored sizes of random lists with ones where we have explicitly counted every branch.

> prop_length :: AVL Int -> Bool
> prop_length xs = count xs == count xs where
>    count Empty = 0
>    count (Single x) = 1
>    count (Branch j _ l r) = count l + count r

Make sure that the heights are correctly calculated.

> prop_height :: AVL Int -> Bool
> prop_height xs = count xs == count xs where
>    count Empty = 0
>    count (Single x) = 0
>    count (Branch _ k l r) = 1 + max (height l) (height r)

And make sure that our sequences stay balanced.

> prop_balanced :: AVL Int -> Bool
> prop_balanced Empty = True
> prop_balanced (Single x) = True
> prop_balanced t@(Branch _ _ l r) =
>      bf t >= -1 && bf t <= 1 && prop_balanced l && prop_balanced r

> -- the balance factor

> bf :: AVL a -> Int
> bf (Branch _ _ l r) = height l - height r
> bf (Single _) = 0
> bf Empty = 0

All three representation invariants together.

> prop_AVL :: AVL Int -> Property
> prop_AVL x = counterexample "length"   (prop_length x)   .&&.
>              counterexample "height"   (prop_height x) .&&.
>              counterexample "balanced" (prop_balanced x)

> -- (f) append

The beauty of this representation is that not only do we get efficient indexing, we also
can append two sequences together in $O(lg n)$ time.

The general idea of the `append` function is that if the heights of `a` and `b` are within 1
of each other, put them together with the branch constructor. Otherwise, if `a` is taller than `b`,
then look along the right spine of `a` for a branch that is balanced with `b`. At that point,
construct a new branch in the tree. However, that part of the tree is now one taller than before,
so it should be rebalanced on the way up. (The case when b is taller than a is analogous.)

> avlAppend :: AVL a -> AVL a -> AVL a
> avlAppend = undefined

Be sure to make sure that append acts like the similar operation on lists

> prop_append :: AVL Int -> AVL Int -> Bool
> prop_append l1 l2 = toList (l1 `append` l2) == toList l1 ++ toList l2

and produces balanced sequences.

> prop_append_AVL :: AVL Int -> AVL Int -> Property
> prop_append_AVL l1 l2 = prop_AVL (avlAppend l1 l2)

> -- (g) Functors and Monads (at last!) 

Like lists, this type can be made an instance of the `Functor`, `Applicative` and `Monad`
type classes. Fill in the details for `Functor` and `Monad` (we have given you the definition
of `Applicative`, which uses the monadic operations). You may find the `Monad` instance for
ordinary lists to be a useful model. But, do not convert AVL trees to ordinary lists in your solution!

> instance Functor AVL where
>    fmap _ _  = error "AVL fmap: unimplemented"

> instance Applicative AVL where
>    pure   = Single
>    (<*>)  = ap  -- this function is defined in terms of bind

> instance Monad AVL where
>    return = error "AVL return: unimplemented"
>    _ >>= _ = error "AVL bind: unimplemented"

How do you know that your Functor and Monad instances are correct? Type classes often come
with laws that govern their correct usage. For example, all implementations of `(==)` should be
reflexive, symmetric, and transitive. Instances that do not follow these laws are confusing and
unpredictable, leading to buggy programs.

Let\'s now write some QuickCheck properties to verify the `Functor` and `Monad` laws. Instead of
`a -> b`, we will use the datatype `Fun a b`, which allows QuickCheck to generate arbitrary function
values. You do not need to understand the details of this, but, if you\'re interested, you can watch
[Koen Claessen's](https://www.youtube.com/watch?v=CH8UQJiv9Q4) talk for background on testing
higher-order functions with QuickCheck.

Inside a property depending on a function `rf :: Fun a b`, we can get the underlying \"real\" function
`f :: a -> b` by pattern matching with `(Fun _ f)`.

Functor instances should satisfy the two laws shown below.

The first law states that mapping the identity function shouldn\'t do anything.

> prop_FMapId :: (Eq (f a), Functor f) => f a -> Bool
> prop_FMapId x = fmap id x == id x

The second law allows us to combine two passes with `fmap` into a single one using function composition.

> prop_FMapComp :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
> prop_FMapComp (Fun _ f) (Fun _ g) x =
>    fmap (f . g) x == (fmap f . fmap g) x

Furthermore, monad instances should satisfy the three monad laws given below.

> prop_LeftUnit :: (Eq (m b), Monad m) => a -> Fun a (m b) -> Bool
> prop_LeftUnit x (Fun _ f) =
>    (return x >>= f) == f x

> prop_RightUnit :: (Eq (m b), Monad m) => m b -> Bool
> prop_RightUnit m =
>    (m >>= return) == m

> prop_Assoc :: (Eq (m c), Monad m) =>
>     m a -> Fun a (m b) -> Fun b (m c) -> Bool

> prop_Assoc m (Fun _ f) (Fun _ g) =
>    ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)

Finally, types that are instances of both `Functor` and `Monad` should satisfy one additional law:

> prop_FunctorMonad :: (Eq (m b), Monad m) => m a -> Fun a b -> Bool
> prop_FunctorMonad x (Fun _ f) = fmap f x == (x >>= return . f)

Now use QuickCheck to verify these properties for your `Functor` and `Monad` instances above.

After you have completed the instances, make sure that your code satisfies the properties by
running the following computations.

> qc1 :: IO ()
> qc1 = quickCheck
>          (prop_FMapId  :: AVL Int -> Bool)

> qc2 :: IO ()
> qc2 = quickCheck
>          (prop_FMapComp :: Fun Int Int -> Fun Int Int -> AVL Int -> Bool)

> qc3 :: IO ()
> qc3 = quickCheck
>          (prop_LeftUnit  :: Int -> Fun Int (AVL Int) -> Bool)

> qc4 :: IO ()
> qc4 = quickCheck (prop_RightUnit :: AVL Int -> Bool)

> qc5 :: IO ()
> qc5 = quickCheck
>            (prop_Assoc :: AVL Int -> Fun Int (AVL Int) -> Fun Int (AVL Int) -> Bool)

> qc6 :: IO ()
> qc6 = quickCheck
>            (prop_FunctorMonad :: AVL Int -> Fun Int (AVL Int) -> Bool)

Furthermore, the `Functor` and `Monad` instances for sequences should be equivalent to the ones
for ordinary lists. More formally, we require following list equalities to hold, no matter what
values are used for `f`, `s`, `x`, `m`, and `k`.

  toList (fmap f s) == fmap f (toList s)
     where s :: AVL a
           f :: a -> b

  toList (return x) == return x
     where x :: a

  toList (m >>= k) == toList m >>= (toList . k)
     where m :: AVL a
           k :: a -> AVL b
           
Use QuickCheck to test that these three identities hold.

> qc7 :: IO ()
> qc7 = undefined

> qc8 :: IO ()
> qc8 = undefined

> qc9 :: IO ()
> qc9 = undefined

Finally, the `Functor` and `Monad` instances for AVL should preserve the AVL invariants.

> qc10 :: IO ()
> qc10 = quickCheck prop_AVL_functor where
>    prop_AVL_functor :: Fun Int Int -> AVL Int -> Property
>    prop_AVL_functor (Fun _ f) x = prop_AVL (fmap f x)

> qc11 :: IO ()
> qc11 = quickCheck prop_AVL_return where
>    prop_AVL_return :: Int -> Property
>    prop_AVL_return x = prop_AVL (return x)

> qc12 :: IO ()
> qc12 = quickCheck prop_AVL_bind where
>    prop_AVL_bind :: AVL Int -> Fun Int (AVL Int) -> Property
>    prop_AVL_bind x (Fun _ k) = prop_AVL (x >>= k)

> qcAVL :: IO()
> qcAVL = qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6 >> qc7 >> qc8 >> qc9 >> qc10 >> qc11 >> qc12

> -- (e)

Now let\'s think about instances of `Functor` and `Monad` for AVL that do not satisfy the laws above.
As a trivial example, if we merely left all of the methods undefined, then quickCheck should easily
return a counterexample. (You might want to verify that it does!)

> {- Invalid instance of Functor and Monad:
> instance Functor AVL where
>    fmap f s = undefined

> instance Monad AVL where
>    return = undefined
>    (>>=)  = undefined
> -}

Are there other invalid instances? Add at least one instance below (in comments) that does not
use undefined or error, and does not include an infinite loop. Your instance(s) should typecheck,
but should fail at least one of the tests above. Please include a note saying which property or
properties are violated.

**Homework Notes**

This problem is inspired by Haskell\'s `Data.Sequence` library. That library uses a data structure
called `FingerTrees` or `Ropes`, which is also based on balanced binary trees, but include additional
structure. In particular, `FingerTrees` provide amortized constant time cons and head and operations.
`FingerTrees` are also more general: besides sequences they can also be used to implement priority queues.

> main :: IO ()
> main = do
>   runTestTT $ TestList [testPairs, testFirst, testFoldable, testAvlIndex,
>              testAvlInsert]
>   quickCheck prop_AVL
>   quickCheck prop_append
>   quickCheck prop_append_AVL
>   qcAVL