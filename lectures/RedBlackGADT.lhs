---
title: Red-Black Trees with GADTs
date: October 18, 2017
---

[RBTlhs]: RedBlackGADT.lhs
[RBT-sol]: RedBlaclGADT-sol.html

*Note:* You may download the [lhs version][RBTlhs]
of this module. Eventually, the [complete version][RBT-sol] will
be made available.

This version of RedBlack trees demonstrates the use of GADTs to
statically verify three of the four red-black invariants.  The final
invariant (the black height constraint) requires a bit more mechanism
and will be handled separately.

> {-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches
>     -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

> {-# LANGUAGE InstanceSigs, GADTs, DataKinds, KindSignatures, ScopedTypeVariables,
>     MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

> module RedBlackGADT where
> import Test.QuickCheck hiding (within)

Here, again, are the invariants for red-black trees:

  1. The empty nodes at the leaves are black.

  2. The root is black.

  3. From each node, every path to a leaf has the same number of black nodes.

  4. Red nodes have black children.

Type structure
--------------

First, we define the data structure for red/black trees.

> data Color = R | B deriving (Eq, Show)

> data T (a :: *) where
>    E :: T a
>    N :: Color -> T a -> a -> T a -> T a
>       deriving (Eq, Show)
>


The type `RBT a` is used at the top of a red-black tree.

> data RBT a where
>   Root :: T a -> RBT a
>


> instance Show a => Show (RBT a) where
>   show (Root x) = show x

> 

> color :: T a -> Color
> color (N c _ _ _) = c
> color E = B


Implementation
--------------

Now we implement the operations of sets using our refined red-black trees.

> empty :: RBT a
> empty = (Root E)

Membership testing and listing the elements in a tree are straightforward.

> member :: Ord a => a -> RBT a -> Bool
> member x (Root t) = aux x t where
>     aux :: Ord a => a -> T a -> Bool
>     aux _ E = False
>     aux x (N _ a y b)
>       | x < y     = aux x a
>       | x > y     = aux x b
>       | otherwise = True

> elements :: Ord a => RBT a -> [a]
> elements (Root t) = aux t [] where
>      aux :: Ord a => T a -> [a] -> [a]
>      aux E acc = acc
>      aux (N _ a x b) acc = aux a (x : aux b acc)

Insertion is naturally a bit trickier...

> insert :: Ord a => a -> RBT a -> RBT a
> insert x (Root t) = blacken (ins x t)

As before, after performing the insertion with the auxiliary function
`ins`, we blacken the top node of the tree to make sure that invariant
(2) is always satisfied.



> blacken :: T a -> RBT a
> blacken (N _ l v r) = Root (N B l v r)

> ins :: Ord a => a -> T a -> T a
> ins x E = N R E x E
> ins x s@(N c a y b)
>   | x < y     = balanceL c (ins x a) y b
>   | x > y     = balanceR c a y (ins x b)
>   | otherwise = (N c a y b)
>

The original `balance` function looked like this:

> {-
> balance (N B (N R (N R a x b) y c) z d) = N R (N B a x b) y (N B c z d)
> balance (N B (N R a x (N R b y c)) z d) = N R (N B a x b) y (N B c z d)

> balance (N B a x (N R (N R b y c) z d)) = N R (N B a x b) y (N B c z d)
> balance (N B a x (N R b y (N R c z d))) = N R (N B a x b) y (N B c z d)
> balance t = t
> -}

The first two clauses handled cases where the left subtree was
unbalanced as a result of an insertion, while the last two handle
cases where a right-insertion has unbalanced the tree.

Here, we split this function in two to recognize that we have information from
`ins` above. We know exactly where to look for the red/red violation! If we
inserted on the left, then we should balance on the left. If we inserted on
the right, then we should balance on the right.

> balanceL :: Color -> T a -> a -> T a -> T a
> balanceL B (N R (N R a x b) y c) z d = N R (N B a x b) y (N B c z d)
> balanceL B (N R a x (N R b y c)) z d = N R (N B a x b) y (N B c z d)
> balanceL col a x b = N col a x b

> balanceR :: Color -> T a -> a -> T a -> T a
> balanceR B a x (N R (N R b y c) z d) = N R (N B a x b) y (N B c z d)
> balanceR B a x (N R b y (N R c z d)) = N R (N B a x b) y (N B c z d)
> balanceR col a x b = N col a x b

>



> 

---

> type Interval a = (Maybe a, Maybe a)

> within :: Ord a => a -> Interval a -> Bool
> within y (Just x, Just z)   = x < y && y < z
> within y (Just x, Nothing)  = x < y
> within y (Nothing, Just z)  = y < z
> within y (Nothing, Nothing) = True

> prop_BST :: RBT Int -> Bool
> prop_BST (Root t) = check (Nothing, Nothing) t where
>    
>    check (min, max) (N _ a x b) = x `within` (min,max)
>                                   && check (min,Just x) a
>                                   && check (Just x,max) b
>    check _ E           = True


> instance (Ord a, Arbitrary a) => Arbitrary (RBT a)  where
>    arbitrary          = foldr insert empty <$> (arbitrary :: Gen [a])