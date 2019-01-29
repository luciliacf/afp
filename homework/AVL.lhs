---
title: Purely Functional Data structures
author: Lucilia Figueiredo
---

[avlhs]: AVL.hs
[avl-sol]: ../code/AVL-sol.zip
[avltree]: https://en.wikipedia.org/wiki/AVL_tree

For this module, please edit the file [AVL.hs][avlhs]. A solution for this homework will eventually be
available [here][avl-sol]. 

> {-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns #-}
> {-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
>
> module AVL (Set(..),AVL(..),
>             avlEmpty,avlElements,avlMember,avlInsert,avlDelete,
>             t1,t2,t3,bad1,bad2,bad3,main,rebalance,height,bf,
>             setProperties,prop_empty,prop_elements,prop_insert1,
>             prop_insert2,prop_delete1,prop_delete2,
>             avlProperties,prop_bst,prop_ht,prop_balance) where
> import Prelude hiding (zipWith,zipWith3)
> import Test.QuickCheck hiding (elements)

The goal of this homework is to implement a purely functional version
of [AVL trees][avltree] in Haskell.

AVL trees are an alternative implementation of balanced binary trees.
Like Red Black trees, they can be used to implement a set data structure.

> class Set s where
>    empty    :: s a
>    member   :: Ord a => a -> s a -> Bool
>    insert   :: Ord a => a -> s a -> s a
>    elements :: s a -> [a]
>    delete   :: Ord a => a -> s a -> s a

Your job in this homework will be to implement and test the following type class instance.

> instance Set AVL where
>    empty    = avlEmpty
>    member   = avlMember
>    insert   = avlInsert
>    elements = avlElements
>    delete   = avlDelete


Set invariants
---------------

Go through the lecture notes and find some general correctness properties about sets.
These properties can be specialized to AVL trees here, but you should only use functions
from the `Set` type class above in their definition. The names and their types are hints,
feel free to change them. You may also add more properties than shown here.
(Make sure you update `setProperties` if necessary.)

> -- the empty set has no elements
> prop_empty :: Bool
> prop_empty = undefined

> -- all elements in the set are distinct
> prop_elements :: AVL Int -> Bool
> prop_elements t = undefined

> -- insetion really inserts the given element and preserves AVL bst and balanced properties
> prop_insert1 :: Int -> AVL Int -> Property
> prop_insert1 x t = undefined

> -- insetion does not remove any element
> prop_insert2 :: Int -> Int -> AVL Int -> Property
> prop_insert2 x y t = undefined

> -- deletion really delets the given element and preserves AVL bst and balanced properties
> prop_delete1 :: Int -> AVL Int -> Property
> prop_delete1 x t = undefined

> -- insetion does not remove any element distinct from the given one
> prop_delete2 :: Int -> Int -> AVL Int -> Property
> prop_delete2 x y t = undefined


NB: The QuickCheck counterexample function names any failing inputs appropriately.

> setProperties :: Property
> setProperties =
>   counterexample "empty"   prop_empty    .&&.
>   counterexample "elts"    prop_elements .&&.
>   counterexample "insert1" prop_insert1  .&&.
>   counterexample "insert2" prop_insert2  .&&.
>   counterexample "delete1" prop_delete1  .&&.
>   counterexample "delete2" prop_delete2 


AVL tree definition & invariants
---------------------------------

AVL trees can be implemented with the following datatype definition.
This definition is similar to that of standard binary trees, the only
difference is that nodes store the height of the tree at that point.

> data AVL e = E           -- empty tree
>            | N           -- non-empty tree
>                Int       -- cached height of the tree
>                (AVL e)   -- left subtree
>                e         -- value
>                (AVL e)   -- right subtree
>   deriving Show

The height of a tree is the maximum distance from a node to any leaf below it.
In a wellformed AVL tree, we should be able to access this component straight off.

> -- | Access the height of the tree
> height :: AVL e -> Int
> height E = 0
> height (N h _ _ _) = h

The balance factor corresponds to the difference in height  between the left subtree and the
right subtree of the node. An invariant of AVL trees is that the balance factor
must be between -1 and 1.

> -- | Calculate the balance factor of a node
> bf :: AVL e -> Int
> bf E = 0
> bf (N _ l _ r) = height l - height r

As the definitions above imply, AVL trees must satisfy specific invariants that ensure that
the tree is balanced. In this problem, you\'ll need to define quickcheck properties
for those invariants.

Of course, AVL trees must be binary search trees.

> -- | The tree is a binary search tree
> prop_bst :: AVL Int -> Bool
> prop_bst = undefined

And they must satisfy the AVL invariants about height and balance.

> -- | The height at each node is correctly calculated.
> prop_ht :: AVL Int -> Bool
> prop_ht = undefined

> -- | The balance factor at each node is between -1 and +1.
> prop_balance :: AVL Int -> Bool
> prop_balance = undefined

> avlProperties :: Property
> avlProperties =
>   counterexample "bst"     prop_bst .&&.
>   counterexample "height"  prop_ht .&&.
>   counterexample "balance" prop_balance

In order to use quick check test these properties of AVL trees, we need to define what it
means for two AVL trees to be equal and also define a generator for arbitrary AVL trees.
Feel free to use any of the functions of the `Set` interface in this implementation, even
if you haven\'t defined those operations for AVL trees yet.

> instance (Eq a) => Eq (AVL a) where
>    (==) = undefined

> instance (Ord e, Arbitrary e) => Arbitrary (AVL e) where
>     arbitrary = undefined
>     shrink = undefined

AVL tree operations
---------------------

Define the first three operations, the empty tree, the function to list the elements of
the AVL tree, and a function to lookup up elements in the tree.

> -- | an empty AVL tree
> avlEmpty :: AVL e
> avlEmpty = undefined

> -- | list the elements in the tree, in order
> avlElements :: AVL e -> [e]
> avlElements t = undefined

> -- | Determine if an element is contained within the tree
> avlMember :: Ord e => e -> AVL e -> Bool
> avlMember = undefined


Sample trees
-------------

Build a few particular trees that you can use as test cases later \-\- some that obey all of
the AVL invariants \.\.\.

> t1 :: AVL Int
> t1 = undefined
> t2 :: AVL Int
> t2 = undefined
> t3 :: AVL Int
> t3 = undefined

\.\.\. and some others that do not \.\.\.

> bad1 :: AVL Int
> bad1 = undefined
> bad2 :: AVL Int
> bad2 = undefined
> bad3 :: AVL Int
> bad3 = undefined

Make sure that you do NOT change the type annotations for these trees.
Your test cases should all have the same type, just some should violate the correctness
properties of the AVL type.


Rebalance
----------

Write a function `rebalance` that takes a tree e whose root node has balance factor -2 or +2
and rearranges it to an equivalent tree that satifies the balance factor invariant.

For this step, you will probably find it helpful to have a good diagram to refer to
(such as the one on Wikipedia.) Note, though, that most explanations of AVL trees
will talk about \"rotating\" the nodes near the root, which implies some sort of pointer
manipulation. Here, we\'re simply rebuilding a completely new tree out of the pieces of
the old one, so the notion of rotating doesn\'t really apply. In particular, you may find
it easier to implement the \"double rotations\" that standard presentations of the algorithm
talk about in a single step.

Even so, a diagram that shows the effect such rotations are trying to achieve is a useful
guide to implementing your rearrangement. I named the variables in my patterns to match
the labels in the diagram I was looking at, and this made it very much easier to write
the rearranged trees correctly.

> -- | Rotate an AVL tree
> rebalance :: (Ord e) => AVL e -> AVL e
> rebalance = undefined

Insert
-------

Write an insertion function for adding new elements to AVL trees.

You should use QuickCheck to verify your implementation of `avlInsert` \-\- both the fact
that it correctly implements insert and that the resulting tree is an AVL tree.

> -- | Insert a new element into a tree, returning a new tree
> avlInsert :: (Ord e) => e -> AVL e -> AVL e
> avlInsert = undefined


Delete
-------

Write a function that removes an element from a tree and rebalances the resulting tree as necessary.
Again, use the properties defined above to test your implementation, making sure that it implements
the deletion operation correctly and preserves the AVL tree properties.

> -- | Delete the provided element from the tree
> avlDelete :: Ord e => e -> AVL e -> AVL e
> avlDelete = undefined

> main :: IO ()
> main = return ()