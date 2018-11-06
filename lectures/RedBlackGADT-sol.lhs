---
title: Red-Black Trees with GADTs
date: October 18, 2017
---

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


In the GADT version of red-black trees, we define colors in two steps.
First, we define a data-_kind_ of colors (which we are going to use to
index types below):

> data Color = Red | Black deriving (Eq, Show)

Then we define a GADT of "singleton colors", where the type of each
constructor records its color at the type level.

> data SColor :: Color -> * where
>    R :: SColor Red
>    B :: SColor Black

(The word "singleton" here refers to the fact that each of the types
`SColor Red` and `SColor Black` describes just one value -- the
constructor `R` is the sole value of type `SColor Red` and `B` is the
only value of `SColor Black`.)

Next, to make sure that red nodes have black children, we introduce a
_multiparameter type class_ called `Valid`. This class captures the
valid relationships between the color of a node and the colors of its
two children. If the former is `Red` the latter must be
`Black`. Alternatively, if the former is `Black` then the latter could
be anything.

> class Valid (c :: Color) (c1 :: Color) (c2 :: Color) where
> instance Valid Red Black Black
> instance Valid Black c1 c2

(Note that the class and instance declarations involve no actual
operations -- we are using the typeclass mechanism not for organizing
groups of methods, as we have in the past, but for constraining
another type declaration.)

Now, here is a GADT for colored trees that captures invariants 1 and
and 4 (we'll deal with 2 below, and we're leaving 3 completely off the
table for the moment):

> data CT (c :: Color) (a :: *) where
>    E  :: CT Black a
>    N  :: Valid c c1 c2 => SColor c -> (CT c1 a) -> a -> (CT c2 a) -> CT c a

The index `c` indicates the color of the top node of the tree.
Datatype promotion allows us to use `Color`s as parameters to type
definitions.  Each data constructor determines what the type parameter
`c` must be.

> 


The type `RBT a` is used at the top of a red-black tree.

> 
> data RBT a where
>   Root :: (CT Black a) -> RBT a
> 


> instance Show a => Show (RBT a) where
>   show (Root x) = show x

> 

Haskell cannot derive the show instances for GADTs, so we provide them
explicitly.

> instance Show (SColor c) where
>    show R = "R"
>    show B = "B"

> instance Show a => Show (CT c a) where
>    show E = "E"
>    show (N c l x r) =
>       "(N " ++ show c ++ " " ++ show l ++ " " ++
>       show x ++ " " ++ show r ++ ")"

Similarly, we define an equality test for singleton colors.

> (%==%) :: SColor c1 -> SColor c2 -> Bool
> R %==% R = True
> B %==% B = True
> _ %==% _ = False

(Notice that it doesn't work to simply declare `SColor` to be an
instance of `Eq`...

    instance Eq (SColor c) where
    R == R = True
    B == B = True
    _ == _ = False

... because the `==` operation defined in this way would have type
`SColor c -> SColor c -> Bool`, which is not what we want: this would
only allow us to compare colors that the typechecker already knew were
the same!)

> 

> 

Last, we define a function for extracting the color of subtree.

> color :: CT c a -> SColor c
> color (N c _ _ _) = c
> color E = B

> 


Implementation
--------------

Now we implement the operations of sets using our refined red-black trees.

> empty :: RBT a
> empty = (Root E)

Membership testing and listing the elements in a tree are straightforward.

> member :: Ord a => a -> RBT a -> Bool
> member x (Root t) = aux x t where
> 
>     aux :: Ord a => a -> CT c a -> Bool
> 
>     aux _ E = False
>     aux x (N _ a y b)
>       | x < y     = aux x a
>       | x > y     = aux x b
>       | otherwise = True

> elements :: Ord a => RBT a -> [a]
> elements (Root t) = aux t [] where
> 
>      aux :: Ord a => CT c a -> [a] -> [a]
> 
>      aux E acc = acc
>      aux (N _ a x b) acc = aux a (x : aux b acc)

Insertion is naturally a bit trickier...

> insert :: Ord a => a -> RBT a -> RBT a
> insert x (Root t) = blacken (ins x t)

As before, after performing the insertion with the auxiliary function
`ins`, we blacken the top node of the tree to make sure that invariant
(2) is always satisfied.


(This is why it was useful to define the
separate "root node type" `RBT`: the type `CT` does not maintain this
invariant, allowing it to represent infrared trees as well, as
required by the insertion algorithm.)


> 
> blacken :: IR a -> RBT a
> blacken (IN _ l v r) = Root (N B l v r)


After insertion into a tree of type `CT c a`, we don't know what color
of tree will be produced.  To capture this, we need an auxiliary type
that hides the top color.

> data IR a where
>   IN :: SColor c -> CT c1 a -> a -> CT c2 a -> IR a

> ins :: Ord a => a -> CT c a -> IR a
> ins x E = IN R E x E
> ins x s@(N c a y b)
>   | x < y     = balanceL c (ins x a) y b
>   | x > y     = balanceR c a y (ins x b)
>   | otherwise = (IN c a y b)

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



> 



To work with the refined types, we need to modify the `balance`
function above. First, we need to break it into two parts, because the
recursive call to insert produces a result of type `IR` not `CT`. Now
we have two balance functions, one for rebalancing after an insertion
in the left subtree (taking an `IR` as its first parameter) and one
for rebalancing after an insertion in the right subtree (taking an
`IR` as second parameter).


> 
> balanceL :: SColor c -> IR a -> a -> CT c1 a -> IR a
> balanceL B (IN R (N R a x b) y c) z d = IN R (N B a x b) y (N B c z d)
> balanceL B (IN R a x (N R b y c)) z d = IN R (N B a x b) y (N B c z d)

The second issue is that we need to be more precise about the cases
when the tree does _not_ need rebalancing. When we're using GADTs, the
type checker looks at each each branch individually; it doesn't track
the ordering of pattern matching.  Thus, we have to match the cases
for already-balanced trees individually, so that all calls to `N` will
satisfy their requirements.

> balanceL col (IN B a x b) z d                         = IN col (N B a x b) z d
> balanceL col (IN R a@(N B _ _ _) x b@(N B _ _ _)) z d = IN col (N R a x b) z d
> balanceL col (IN R a@E x b@E) z d                     = IN col (N R a x b) z d
> -- DON't need these, they violate the black height
> -- balanceL col (IN R a@E x b@(N B _ _ _)) z d           = IN col (N R a x b) z d
> -- balanceL col (IN R a@(N B _ _ _) x b@E) z d           = IN col (N R a x b) z d


Similarly...

> balanceR :: SColor c -> CT c1 a -> a -> IR a -> IR a
> balanceR B a x (IN R (N R b y c) z d) = IN R (N B a x b) y (N B c z d)
> balanceR B a x (IN R b y (N R c z d)) = IN R (N B a x b) y (N B c z d)
> balanceR c a x (IN B b z d)           = IN c a x (N B b z d)
> balanceR c a x (IN R b@(N B _ _ _) z d@(N B _ _ _)) = IN c a x (N R b z d)
> balanceR c a x (IN R b@E z d@(N B _ _ _)) = IN c a x (N R b z d)
> balanceR c a x (IN R b@(N B _ _ _) z d@E) = IN c a x (N R b z d)
> balanceR c a x (IN R b@E z d@E) = IN c a x (N R b z d)

The file [RedBlackGADT1](RedBlackGADT1.html), which can be downloaded
[here](RedBlackGADT1.lhs) further refines this implementation to
include the black-height invariant.

Some notes for further reading can be found at the bottom of that file.



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
>    check :: Interval Int -> CT c Int -> Bool
>    
>    check (min, max) (N _ a x b) = x `within` (min,max)
>                                   && check (min,Just x) a
>                                   && check (Just x,max) b
>    check _ E           = True


> instance (Ord a, Arbitrary a) => Arbitrary (RBT a)  where
>    arbitrary          = foldr insert empty <$> (arbitrary :: Gen [a])