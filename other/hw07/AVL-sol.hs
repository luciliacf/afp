{-# OPTIONS -fwarn-tabs -fwarn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module AVL (Set(..),AVL(..),
            avlEmpty,avlElements,avlMember,avlInsert,avlDelete,
            t1,t2,t3,bad1,bad2,bad3,main,rebalance,height,bf,
            setProperties,prop_empty,prop_elements,prop_insert1,
            prop_insert2,prop_delete1,prop_delete2,
            avlProperties,prop_bst,prop_ht,prop_balance) where

import Data.List hiding (delete,insert)
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

-- the empty set has no elements
prop_empty :: Bool
prop_empty = null (elements (empty :: AVL Int))

-- all elements in the set are distinct
prop_elements :: AVL Int -> Bool
prop_elements t = length (nub l) == length l
  where l = elements t 

-- insertion really inserts the given element and preserves AVL bst and balanced properties
prop_insert1 :: Int -> AVL Int -> Property
prop_insert1 x t = prop_bst t && prop_balance t ==> member x t' && prop_bst t' && prop_balance t'
  where t' = insert x t

-- insetion does not remove any element
prop_insert2 :: Int -> Int -> AVL Int -> Property
prop_insert2 x y t = x /= y ==> member y t == member y (insert x t)

-- deletion really delets the given element and preserves AVL bst and balanced properties
prop_delete1 :: Int -> AVL Int -> Property
prop_delete1 x t = prop_bst t && prop_balance t ==> not (member x t') && prop_bst t' && prop_balance t'
  where t' = insert x t

-- insetion does not remove any element distinct from the given one
prop_delete2 :: Int -> Int -> AVL Int -> Property
prop_delete2 x y t = x /= y ==> member y t == member y (delete x t)


setProperties :: Property
setProperties =
  counterexample "empty"   prop_empty    .&&.
  counterexample "elts"    prop_elements .&&.
  counterexample "insert1" prop_insert1  .&&.
  counterexample "insert2" prop_insert2  .&&.
  counterexample "delete1" prop_delete1  .&&.
  counterexample "delete2" prop_delete2

-- AVL tree definitions and invariants

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
bf (N _ l _ r) = height r - height l

-- | The tree is a binary search tree
prop_bst :: AVL Int -> Bool
prop_bst E           = True
prop_bst (N _ E _ E) = True 
prop_bst (N _ E v r@(N hr lr vr rr)) = vr > v && prop_bst r
prop_bst (N _ l@(N hl ll vl lr) v E) = vl < v && prop_bst l
prop_bst (N _ l@(N hl ll vl rl) v r@(N hr lr vr rr)) = vr > v && vl < v && prop_bst l && prop_bst r

-- | The height at each node is correctly calculated.
prop_ht :: AVL Int -> Bool
prop_ht t = case t of
              E            -> height(t) == 0
              (N h l _ r ) -> height(t) == height(l) `max` height(r) + 1 && prop_ht l && prop_ht r

-- | The balance factor at each node is between -1 and +1.
prop_balance :: AVL Int -> Bool
prop_balance E           = True
prop_balance (N _ l _ r) = bal <= 1 where bal = abs (height(l) - height(r))

avlProperties :: Property
avlProperties =
  counterexample "bst"     prop_bst .&&.
  counterexample "height"  prop_ht .&&.
  counterexample "balance" prop_balance

instance (Eq a) => Eq (AVL a) where
  E               == E               = True
  (N h1 l1 v1 r1) == (N h2 l2 v2 r2) = h1 == h2 && v1 == v2 && l1 == l2 && r1 == r2
  _               == _               = False
  
instance (Ord e, Arbitrary e) => Arbitrary (AVL e) where
    arbitrary = foldr insert E <$> (arbitrary :: Gen [e])
    shrink E           = []
    shrink (N _ l _ r) = [l, r]
    
-- 3

-- | an empty AVL tree
avlEmpty :: AVL e
avlEmpty = E

-- | list the elements in the tree, in order
avlElements :: AVL e -> [e]
avlElements = elts []
  where elts xs E           = xs
        elts xs (N h l v r) = elts (v: elts xs r) l 

-- | Determine if an element is contained within the tree
avlMember :: Ord e => e -> AVL e -> Bool
avlMember _ E = False
avlMember x (N h l v r)
   | x == v     = True
   | x < v      = avlMember x l
   | otherwise  = avlMember x r

-- | Construct an AVL tree leaf with the given value
avlLeaf :: e -> AVL e
avlLeaf x = N 1 E x E

-- | Access the value of the tree
avlval (N _ _ v _) = v
avlval E           = undefined

-- | Access the left tree of the given tree
left (N _ l _ _) = l
left E           = undefined

-- | Access the right tree of the given tree
right (N _ _ _ r) = r
right E           = undefined

-- 4

t1 :: AVL Int
t1 = N 4
       (N 3 (N 1 E 4 E) 7 (N 2 (N 1 E 11 E) 12 (N 1 E 13 E)))
       14
       (N 2 E 17 (N 1 E 30 E))

t2 :: AVL Int
t2 = N 4
       (N 3 (N 1 E 4 E) 7 (N 2 E 11 (N 1 E 13 E)))
       14
       (N 2 E 17 (N 1 E 30 E))

t3 :: AVL Int
t3 = N 4
       (N 3 (N 2 (N 1 E 4 E) 7 (N 1 E 8 E)) 11 (N 2 E 11 (N 1 E 13 E)))
       14
       (N 2 E 17 (N 1 E 30 E))


bad1 :: AVL Int -- unbalanced
bad1 = N 5
       (N 4 (N 1 E 4 E) 7 (N 3 (N 1 E 8 E) 11 (N 2 E 12 (N 1 E 13 E))))
       14
       (N 2 E 17 (N 1 E 30 E))


bad2 :: AVL Int -- not bst
bad2 = N 4
       (N 3 (N 2 (N 1 E 4 E) 30 (N 1 E 8 E)) 11 (N 2 E 11 (N 1 E 13 E)))
       14
       (N 2 E 17 (N 1 E 3 E))

bad3 :: AVL Int -- height not correct and not balanced
bad3 = N 2
       (N 6 (N 2 (N 1 E 4 E) 7 (N 1 E 8 E)) 11 (N 2 E 11 (N 1 E 13 E)))
       14
       (N 3 E 17 (N 1 E 30 E))

-- 5

-- | Rotate an AVL tree
rebalance :: (Ord e) => AVL e -> AVL e
rebalance E = E
rebalance t@(N _ l x r)
  | abs (bf t) <= 1 = t -- already balanced tree
  | abs (bf l) > 1  = -- the deepest unbalanced subtree is to the left
                      let {l' = rebalance l; h = height l' `max` height r + 1} in N h l' x r 
  | abs (bf r) > 1 = -- the deepest unbalanced subtree is to the right
                     let {r'= rebalance r; h = height l `max` height r' + 1} in N h l x r'
  | otherwise = -- t is the deepest unbalanced subtree
                if height r >  height l  -- the left subtree is longer than the right one
                then if height (right r) > height (left r) 
                     then   -- leftrotate
                           let {l'= N hl l x (left r); hl = height l `max` height (left r) + 1;
                                v' = avlval r; r' = right r; h = height l' `max` height r' + 1} 
                           in N h l' v' r'  
                     else   -- leftrightrotate
                           let {l'= N hl l x (left (left r)); v'= avlval (left r);
                               r' = N hr (right (left r)) (avlval r) (right r);
                               hl = height l `max` height (left(left r)) + 1;
                               hr = height (right (left r)) `max` height (right r) + 1;
                               h = height l' `max` height r' + 1 } 
                          in N h l' v' r'   
                else                     -- the right subtree is longer than the left one
                     if height (left l) > height (right l)         
                     then -- rightrotate
                          let {r'= N (height (right l) `max` height r + 1) (right l) x r ;
                               v' = avlval l; l' = left l; h = height l' `max` height r' + 1}
                          in N h l' v' r'  

                     else -- rightleftrotate
                          let {l' = N hl (left l) (avlval l) (left (right l));
                               v' = avlval (right l);
                               r' = N hr (right (right l)) x r; 
                               hl = height (left l) `max` height (left (right l)) + 1;
                               hr = height (right(right l)) `max` height r + 1;
                               h = height l' `max` height r' + 1 } 
                          in N h l' v' r'    

 -- 6

-- | Insert a new element into a tree, returning a new tree
avlInsert :: (Ord e) => e -> AVL e -> AVL e
avlInsert x E = avlLeaf x
avlInsert x (N h l v r)
   | x < v     = let {l' = avlInsert x l; h' = height l' `max` height r + 1}
                 in  rebalance $ (N h' l' v r) --  
   | x > v     = let {r' = avlInsert x r; h' = height l `max` height r' + 1}
                 in  rebalance $ (N h' l v r') -- 
   | otherwise = N h l v r

-- 7

-- | Delete the minimum element of the tree
avlDeleteMin :: AVL e -> (e, AVL e)
avlDeleteMin (N _ E v r) = (v,r)
avlDeleteMin (N _ l v r) = let {(v',l') = avlDeleteMin l; h = height l' `max` height r + 1}
                           in (v', N h l' v r)
avlDeleteMin E = undefined

-- | Delete the maximum element of the tree
avlDeleteMax :: AVL e -> (e, AVL e)
avlDeleteMax (N _ l v E) = (v,l)
avlDeleteMax (N _ l v r) = let {(v',r') = avlDeleteMax r; h = height l `max` height r' + 1}
                           in (v', N h l v r')
avlDeleteMax E = undefined

-- | Delete the provided element from the tree
avlDelete :: Ord e => e -> AVL e -> AVL e
avlDelete x E = E
avlDelete x t@(N _ E v E)
  | x == v    = E
  | otherwise = t
avlDelete x (N _ l v r)
  | x < v     = let {l' = avlDelete x l; h' = height l' `max` height r + 1}
                in  rebalance $ (N h' l' v r)  
  | x > v     = let {r' = avlDelete x r; h' = height l `max` height r' + 1}
                in  rebalance $ (N h' l v r') 
  | otherwise = if not(l==E)
                then let {(v',l') = avlDeleteMax l; h' = height l' `max` height r + 1}
                     in rebalance $ N h' l' v' r
                else let {(v',r') = avlDeleteMin r; h' = height l `max` height r' + 1}
                     in rebalance $ N h' l v' r'

main :: IO ()
main = return ()
