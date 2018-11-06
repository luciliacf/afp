---
title: Red-Black Trees with GADTs (BlackHeight)
date: October 18, 2017
---

This version of RedBlack trees demonstrates the use of GADTs to
statically verify all four red-black invariants.

> {-# OPTIONS_GHC  -fno-warn-unused-binds -fno-warn-unused-matches
>     -fno-warn-name-shadowing -fno-warn-missing-signatures #-}

> {-# LANGUAGE InstanceSigs, GADTs, DataKinds, KindSignatures, ScopedTypeVariables,
>     MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

> module RedBlackGADTBlackHeight where
> import Test.QuickCheck hiding (within)

Here, again, are the invariants for red-black trees:

  1. The empty nodes at the leaves are black.

  2. The root is black.

  3. From each node, every path to a leaf has the same number of black nodes.

  4. Red nodes have black children.

Type structure
--------------

First, we define the data structure for red/black trees.

> data Nat = Z | S Nat

> data Color = Red | Black deriving (Eq, Show)

> data SColor (c :: Color) where
>    R :: SColor Red
>    B :: SColor Black


> instance Eq (SColor c) where
>    R == R = True
>    B == B = True
> --   _ == _ = False

> data T (n :: Nat) (c :: Color) (a :: *) where
>    E :: T Z Black a
>    N :: (Valid c c1 c2) => SColor c -> T n c1 a -> a -> T n c2 a -> T (Incr c n) c a
>  --     deriving (Eq, Show)

> type family Incr (c :: Color) (n :: Nat) where
>    Incr Black n = S n
>    Incr Red   n = n


> class Valid (c1 :: Color) (c2 :: Color) (c3 :: Color)
> instance Valid Black c2 c3
> instance Valid Red Black Black


> instance Show (SColor c) where
>    show R = "R"
>    show B = "B"

> instance Show a => Show (T n c a) where
>    show E = "E"
>    show (N c l x r) = "(N " ++ show c ++ " " ++ show l ++ " " ++ show x ++ " " ++ show r ++ show ")"





The type `RBT a` is used at the top of a red-black tree.

> data RBT a where
>   Root :: T n Black a -> RBT a



> instance Show a => Show (RBT a) where
>   show (Root x) = show x



> color :: T n c a -> SColor c
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
>     aux :: Ord a => a -> T n c a -> Bool
>     aux _ E = False
>     aux x (N _ a y b)
>       | x < y     = aux x a
>       | x > y     = aux x b
>       | otherwise = True

> elements :: Ord a => RBT a -> [a]
> elements (Root t) = aux t [] where
>      aux :: Ord a => T n c a -> [a] -> [a]
>      aux E acc = acc
>      aux (N _ a x b) acc = aux a (x : aux b acc)

Insertion is naturally a bit trickier...

> insert :: Ord a => a -> RBT a -> RBT a
> insert x (Root t) = blacken (ins x t)

As before, after performing the insertion with the auxiliary function
`ins`, we blacken the top node of the tree to make sure that invariant
(2) is always satisfied.

> data IR n a where
>   IN :: SColor c -> T n c1 a -> a -> T n c2 a -> IR (Incr c n) a

> blacken :: IR n a -> RBT a
> blacken (IN _ l v r) = Root (N B l v r)

> ins :: forall a n c. Ord a => a -> T n c a -> IR n a
> ins x E = IN R E x E
> ins x s@(N c a y b)
>   | x < y     = balanceL c (ins x a) y b
>   | x > y     = balanceR c a y (ins x b)
>   | otherwise = (IN c a y b)


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

> balanceL :: SColor c -> IR n a -> a -> T n c1 a -> IR (Incr c n) a
> balanceL B (IN R (N R a x b) y c) z d = IN R (N B a x b) y (N B c z d)
> balanceL B (IN R a x (N R b y c)) z d = IN R (N B a x b) y (N B c z d)

> balanceL col a@(IN B a1 y a2) x b = IN col (N B a1 y a2) x b
> balanceL col a@(IN R a1@E y a2@E) x b = IN col (N R a1 y a2) x b
> balanceL col a@(IN R a1@(N B _ _ _) y a2@(N B _ _ _)) x b = IN col (N R a1 y a2) x b

> balanceR :: SColor c -> T n c1 a -> a -> IR n a -> IR (Incr c n) a
> balanceR B a x (IN R (N R b y c) z d) = IN R (N B a x b) y (N B c z d)
> balanceR B a x (IN R b y (N R c z d)) = IN R (N B a x b) y (N B c z d)
> balanceR col a x b@(IN B b1 y b2) = IN col a x (N B b1 y b2)
> balanceR col a x b@(IN R b1@E y b2@E) = IN col a x (N R b1 y b2)
> balanceR col a x b@(IN R b1@(N B _ _ _) y b2@(N B _ _ _)) = IN col a x (N R b1 y b2)

---

> type Interval a = (Maybe a, Maybe a)

> within :: Ord a => a -> Interval a -> Bool
> within y (Just x, Just z)   = x < y && y < z
> within y (Just x, Nothing)  = x < y
> within y (Nothing, Just z)  = y < z
> within y (Nothing, Nothing) = True

> prop_BST :: RBT Int -> Bool
> prop_BST (Root t) = check (Nothing, Nothing) t where
>    check :: Ord a => Interval a -> T n c a -> Bool
>    check (min, max) (N _ a x b) = x `within` (min,max)
>                                   && check (min,Just x) a
>                                   && check (Just x,max) b
>    check _ E           = True


> instance (Ord a, Arbitrary a) => Arbitrary (RBT a)  where
>    arbitrary          = foldr insert empty <$> (arbitrary :: Gen [a])