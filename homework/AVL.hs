{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module AVL (Set(..),AVL(..),
            avlEmpty,avlElements,avlMember,avlInsert,avlDelete,
            t1,t2,t3,bad1,bad2,bad3,main,rebalance,height,bf,
            setProperties,prop_empty,prop_elements,prop_insert1,
            prop_insert2,prop_delete1,prop_delete2,prop_delete3,
            avlProperties,prop_bst,prop_ht,prop_balance) where
import Prelude hiding (zipWith,zipWith3)
import Test.QuickCheck hiding (elements)


class Set s where
   empty    :: s a
   member   :: Ord a => a -> s a -> Bool
   insert   :: Ord a => a -> s a -> s a
   elements :: s a -> [a]
   delete   :: Ord a => a -> s a -> s a

instance Set AVL where
   empty    = avlEmpty
   member   = avlMember
   insert   = avlInsert
   elements = avlElements
   delete   = avlDelete

-- 1

prop_empty :: Bool
prop_empty = undefined

prop_elements :: AVL Int -> Bool
prop_elements x = undefined

prop_insert1 :: Int -> AVL Int -> Bool
prop_insert1 x t = undefined

prop_insert2 :: Int -> AVL Int -> Bool
prop_insert2 x t = undefined

prop_delete1 :: AVL Int -> Bool
prop_delete1 t = undefined

prop_delete2 :: AVL Int -> Bool
prop_delete2 t = undefined

prop_delete3 :: AVL Int -> Int -> Property
prop_delete3 t x = undefined

setProperties :: Property
setProperties =
  counterexample "empty"   prop_empty    .&&.
  counterexample "elts"    prop_elements .&&.
  counterexample "insert1" prop_insert1  .&&.
  counterexample "insert2" prop_insert2  .&&.
  counterexample "delete1" prop_delete1  .&&.
  counterexample "delete2" prop_delete2  .&&.
  counterexample "delete3" prop_delete3

-- 2

data AVL e = E           -- empty tree
           | N           -- non-empty tree
               Int       -- cached height of the tree
               (AVL e)   -- left subtree
               e         -- value
               (AVL e)   -- right subtree
  deriving Show

-- | Access the height of the tree
height :: AVL e -> Int
height E = 0
height (N h _ _ _) = h

-- | Calculate the balance factor of a node
bf :: AVL e -> Int
bf E = 0
bf (N _ l _ r) = height l - height r

-- | The tree is a binary search tree
prop_bst :: AVL Int -> Bool
prop_bst = undefined

-- | The height at each node is correctly calculated.
prop_ht :: AVL Int -> Bool
prop_ht = undefined

-- | The balance factor at each node is between -1 and +1.
prop_balance :: AVL Int -> Bool
prop_balance = undefined

avlProperties :: Property
avlProperties =
  counterexample "bst"     prop_bst .&&.
  counterexample "height"  prop_ht .&&.
  counterexample "balance" prop_balance

instance (Eq a) => Eq (AVL a) where
   (==) = undefined

instance (Ord e, Arbitrary e) => Arbitrary (AVL e) where
    arbitrary = undefined
    shrink = undefined

-- 3

-- | an empty AVL tree
avlEmpty :: AVL e
avlEmpty = undefined

-- | list the elements in the tree, in order
avlElements :: AVL e -> [e]
avlElements t = undefined

-- | Determine if an element is contained within the tree
avlMember :: Ord e => e -> AVL e -> Bool
avlMember = undefined

-- 4

t1 :: AVL Int
t1 = undefined

t2 :: AVL Int
t2 = undefined

t3 :: AVL Int
t3 = undefined

bad1 :: AVL Int
bad1 = undefined

bad2 :: AVL Int
bad2 = undefined

bad3 :: AVL Int
bad3 = undefined

-- 5



-- | Rotate an AVL tree
rebalance :: (Ord e) => AVL e -> AVL e
rebalance = undefined

 -- 6

-- | Insert a new element into a tree, returning a new tree
avlInsert :: (Ord e) => e -> AVL e -> AVL e
avlInsert = undefined

-- 7

-- | Delete the provided element from the tree
avlDelete :: Ord e => e -> AVL e -> AVL e
avlDelete = undefined



main :: IO ()
main = return ()
