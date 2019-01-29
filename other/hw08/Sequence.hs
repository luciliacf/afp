{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE MonadComprehensions, ScopedTypeVariables #-}

module Sequence where

import Test.HUnit hiding (State)
import Test.QuickCheck
import Test.QuickCheck.Function

import Data.Maybe
import Control.Applicative(Alternative(..))
import Control.Monad(ap, liftM, liftM2, guard, forM, foldM)

class (Monad l, Foldable l) => Sequence l where
   -- construction
   nil    :: l a
   single :: a -> l a
   append :: l a -> l a -> l a
   -- position based operations
   first  :: l a -> Maybe a
   final  :: l a -> Maybe a
   index  :: Int -> l a -> Maybe a
   insert :: Int -> a -> l a -> Maybe (l a)

pairs :: Sequence l => l a -> l b -> l (a,b)
pairs xs ys = [(x,y) | x <- xs, y <- ys ]

instance Sequence [] where
   nil        = []
   single x     = [x]
   append       = (++)
   first l      = guard (not (null l)) >> return (head l)
   final l      = guard (not (null l)) >> return (last l)
   index n l    = guard (0 <= n && n < length l) >> return (l !! n)
   insert n x l = guard (0 <= n && n < length l) >> return (before ++ x : after)
      where
       (before, after) = splitAt n l

data AVL a =
     Empty
   | Single a
   | Branch
         Int       -- cached number of elements
         Int       -- cached height
         (AVL a)   -- left child
         (AVL a)   -- right child
       deriving (Show)

seq1 :: AVL Int
seq1 = Branch 4 2 (Branch 2 1 (Single 7) (Single 3))
                  (Branch 2 1 (Single 4) (Single 5))

instance Sequence AVL where
   nil        = Empty
   single     = Single
   append     = avlAppend
   first      = avlFirst
   final      = avlFinal
   index      = avlIndex
   insert     = avlInsert

testPairs :: Test
testPairs = "pairs" ~: toList (pairs seq1 seq1) ~=?
  [(7,7),(7,3),(7,4),(7,5),(3,7),(3,3),(3,4),(3,5),
   (4,7),(4,3),(4,4),(4,5),(5,7),(5,3),(5,4),(5,5)]

-- (a) first and final

-- | access the first element of the sequence, if there is one.
avlFirst :: AVL a -> Maybe a
avlFirst Empty                = Nothing
avlFirst (Single x)           = Just x
avlFirst (Branch _ _ Empty r) = avlFirst r
avlFirst (Branch _ _ l _)     = avlFirst l

-- | access the last element of the list, if there is one (similar to above)
avlFinal :: AVL a -> Maybe a
avlFinal Empty                = Nothing
avlFinal (Single x)           = Just x
avlFinal (Branch _ _ l Empty) = avlFinal l
avlFinal (Branch _ _ _ r)     = avlFinal r

testFirst :: Test
testFirst = TestList [ "first" ~: first seq1 ~=? Just 7,
                       "final" ~: final seq1 ~=? Just 5]

-- (b) Reducing sequences

instance Foldable AVL where
 -- The default definition of the length function looks something like this:
 --   length = foldr (\x s -> s +1) 0
   -- Override this definition with an optimized version that is O(1)
   length Empty            = 0
   length (Single _)       = 1
   length (Branch n _ _ _) = n

   -- Finish the `foldr` definition below so that it is O(n) (Hint: see HW2)
   foldr f b Empty            = b
   foldr f b (Single x)       = f x b
   foldr f b (Branch _ _ xs ys) = foldr f (foldr f b ys) xs 

-- | toList 
toList :: Sequence l => l a -> [a]
toList = foldr (:) []

-- | fromList
insFromList :: Sequence l => l a -> Int -> [a] -> l a
insFromList s n xs = fromJust $ foldM (flip $ \(i,v) -> insert i v) s (zip [n..] xs)

fromList :: Sequence l => [a] -> l a
fromList = insFromList nil 0

instance Eq a => Eq (AVL a) where
   l1 == l2 = toList l1 == toList l2

testFoldable :: Test
testFoldable =
    TestList [ "length" ~: length seq1 ~?= 4
             , "toList" ~: toList seq1 ~?= [7,3,4,5]
             , "sum"    ~: sum    seq1 ~?= 19
             ]

-- (c)  Indexing

avlIndex :: Int -> AVL a -> Maybe a
avlIndex _ Empty            = Nothing
avlIndex 0 (Single x)       = Just x
avlIndex n (Single _)       = Nothing
avlIndex n (Branch k h l r)
   | n < k'     = avlIndex n l
   | n < k      = avlIndex n' r
   | otherwise  = Nothing
   where {k' = length l; n' = n - k'}

testAvlIndex = TestList [ "index 0"  ~: avlIndex  0 seq1 ~?= Just 7,
                          "index 1"  ~: avlIndex  1 seq1 ~?= Just 3,
                          "index 2"  ~: avlIndex  2 seq1 ~?= Just 4,
                          "index 3"  ~: avlIndex  3 seq1 ~?= Just 5 ]

-- (d) Insert

branch :: AVL a -> AVL a -> AVL a
branch x y = Branch (length x + length y) (1 + max (height x) (height y)) x y

height :: AVL a -> Int
height Empty = 0
height (Single x) = 0
height (Branch _ k s1 s2) = k

left :: AVL a -> AVL a
left (Branch _ _ l _) = l
left _                = undefined

right :: AVL a -> AVL a
right (Branch _ _ _ r) = r
right _                = undefined

-- the balance factor
bf :: AVL a -> Int
bf (Branch _ _ l r) = height l - height r
bf (Single _) = 0
bf Empty = 0

rebalance :: AVL a -> AVL a
rebalance t@(Branch _ _ l r)
  | abs(bf l) > 1 = branch (rebalance l) r
  | abs(bf r) > 1 = branch l (rebalance r)
  | bf t < -1     = if height (left r) <= height (right r)
                    then branch (branch l (left r)) (right r)
                    else branch (branch l (left (left r))) (branch (right (left r)) ( right r))
  | bf t > 1      = if height (right l) <= height (left l)
                    then branch (left l) (branch (right l) r)
                    else branch (branch (left l) (left (right l))) (branch (right (right l)) r)
  | otherwise     = t
rebalance t = t

avlInsert :: Int -> a -> AVL a -> Maybe (AVL a)
avlInsert 0 x Empty = return (Single x)
avlInsert n _ Empty = Nothing
avlInsert n x (Single y)
   | n==0      = return (Branch 2 1 (Single x) (Single y))
   | n==1      = return (Branch 2 1 (Single y) (Single x))
   | otherwise = Nothing
avlInsert n x (Branch k h l r)
   | n < k'    = do {l' <- (avlInsert n x l);  return (rebalance $ branch l' r)}
   | n <= k    = do {r' <- (avlInsert n' x r); return (rebalance $ branch l r')}
   | otherwise = Nothing
   where {k' = length l; n' = n - k'}
   
testAvlInsert :: Test
testAvlInsert = TestList [
    "insert 0 " ~: toList <$> insert 0 1 seq1 ~?= Just [1,7,3,4,5]
  , "insert 1 " ~: toList <$> insert 1 1 seq1 ~?= Just [7,1,3,4,5]
  , "insert 2 " ~: toList <$> insert 2 1 seq1 ~?= Just [7,3,1,4,5]
  , "insert 3 " ~: toList <$> insert 3 1 seq1 ~?= Just [7,3,4,1,5]
  , "insert 4 " ~: toList <$> insert 4 1 seq1 ~?= Just [7,3,4,5,1]
  ]

-- (e) Testing with quickcheck

instance (Show a, Arbitrary a) => Arbitrary (AVL a) where
    arbitrary = do { xs <- arbitrary :: Arbitrary e => Gen [e]; return (fromList xs)}
    shrink Empty            = []
    shrink (Single x)       = [Single x]
    shrink (Branch _ _ l r) = [l,r]

prop_length :: AVL Int -> Bool
prop_length xs = length xs == count xs where
   count Empty = 0
   count (Single x) = 1
   count (Branch j _ l r) = count l + count r

prop_height :: AVL Int -> Bool
prop_height xs = height xs == count xs where
   count Empty = 0
   count (Single x) = 0
   count (Branch _ k l r) = 1 + max (height l) (height r)

prop_balanced :: AVL Int -> Bool
prop_balanced Empty = True
prop_balanced (Single x) = True
prop_balanced t@(Branch _ _ l r) =
     bf t >= -1 && bf t <= 1 && prop_balanced l && prop_balanced r

prop_AVL :: AVL Int -> Property
prop_AVL x = counterexample "length"   (prop_length x)   .&&.
             counterexample "height"   (prop_height x) .&&.
             counterexample "balanced" (prop_balanced x)

-- (f) append

avlAppend :: AVL a -> AVL a -> AVL a
avlAppend x y
  | kx < ky   = insFromList y 0  (toList x)
  | otherwise = insFromList x kx (toList y)
  where {kx = length x; ky = length y}

prop_append :: AVL Int -> AVL Int -> Bool
prop_append l1 l2 = toList (l1 `append` l2) == toList l1 ++ toList l2

prop_append_AVL :: AVL Int -> AVL Int -> Property
prop_append_AVL l1 l2 = prop_AVL (avlAppend l1 l2)

-- Functors and Monads (at last!) (g)

instance Functor AVL where
   fmap f Empty            = Empty
   fmap f (Single x)       = Single (f x)
   fmap f (Branch k h l r) = Branch k h (fmap f l) (fmap f r)

instance Applicative AVL where
   pure   = Single
   (<*>)  = ap     -- this function is defined in terms of bind

instance Monad AVL where
   return = Single 
   Empty            >>= f = Empty
   (Single x)       >>= f = f x
   (Branch k h l r) >>= f = avlAppend (l >>= f) (r >>= f)

prop_FMapId :: (Eq (f a), Functor f) => f a -> Bool
prop_FMapId x = fmap id x == id x

prop_FMapComp :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
prop_FMapComp (Fun _ f) (Fun _ g) x =
   fmap (f . g) x == (fmap f . fmap g) x

prop_LeftUnit :: (Eq (m b), Monad m) => a -> Fun a (m b) -> Bool
prop_LeftUnit x (Fun _ f) =
   (return x >>= f) == f x

prop_RightUnit :: (Eq (m b), Monad m) => m b -> Bool
prop_RightUnit m =
   (m >>= return) == m

prop_Assoc :: (Eq (m c), Monad m) =>
    m a -> Fun a (m b) -> Fun b (m c) -> Bool
prop_Assoc m (Fun _ f) (Fun _ g) =
   ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)

prop_FunctorMonad :: (Eq (m b), Monad m) => m a -> Fun a b -> Bool
prop_FunctorMonad x (Fun _ f) = fmap f x == (x >>= return . f)

qc1 :: IO ()
qc1 = quickCheck
         (prop_FMapId  :: AVL Int -> Bool)

qc2 :: IO ()
qc2 = quickCheck
         (prop_FMapComp :: Fun Int Int -> Fun Int Int -> AVL Int -> Bool)

qc3 :: IO ()
qc3 = quickCheck
         (prop_LeftUnit  :: Int -> Fun Int (AVL Int) -> Bool)

qc4 :: IO ()
qc4 = quickCheck (prop_RightUnit :: AVL Int -> Bool)

qc5 :: IO ()
qc5 = quickCheck
           (prop_Assoc :: AVL Int -> Fun Int (AVL Int) -> Fun Int (AVL Int) -> Bool)

qc6 :: IO ()
qc6 = quickCheck
           (prop_FunctorMonad :: AVL Int -> Fun Int (AVL Int) -> Bool)

prop_eqvList1 :: (Sequence l, Eq b) => Fun a b -> l a -> Bool
prop_eqvList1 (Fun _ f) s = toList (fmap f s) == fmap f (toList s)

qc7 :: IO ()
qc7 = quickCheck (prop_eqvList1 :: Fun Int (AVL Int) -> AVL Int -> Bool)

prop_eqvList2 :: Eq a => a -> Bool
prop_eqvList2 x = toList ((return :: a -> AVL a) x) == return x

qc8 :: IO ()
qc8 = quickCheck (prop_eqvList2 :: Int -> Bool)

prop_eqvList3 :: (Sequence l, Eq b) => l a -> Fun a (l b) -> Bool
prop_eqvList3 m (Fun _ k) = toList (m >>= k) == (toList m >>= (toList . k))

qc9 :: IO ()
qc9 = quickCheck (prop_eqvList3 :: AVL Int -> Fun Int (AVL Int) -> Bool)

qc10 :: IO ()
qc10 = quickCheck prop_AVL_functor where
   prop_AVL_functor :: Fun Int Int -> AVL Int -> Property
   prop_AVL_functor (Fun _ f) x = prop_AVL (fmap f x)

qc11 :: IO ()
qc11 = quickCheck prop_AVL_return where
   prop_AVL_return :: Int -> Property
   prop_AVL_return x = prop_AVL (return x)

qc12 :: IO ()
qc12 = quickCheck prop_AVL_bind where
   prop_AVL_bind :: AVL Int -> Fun Int (AVL Int) -> Property
   prop_AVL_bind x (Fun _ k) = prop_AVL (x >>= k)

qcAVL :: IO()
qcAVL = qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6 >> qc7 >> qc8 >> qc9 >> qc10 >> qc11 >> qc12

-- (e)

{- Invalid instance of Functor and Monad:

instance Functor AVL where
   fmap f s = undefined
instance Monad AVL where
   return = undefined
   (>>=)  = undefined
-}



main :: IO ()
main = do
  runTestTT $ TestList [testPairs, testFirst, testFoldable, testAvlIndex,
             testAvlInsert]
  quickCheck prop_AVL
  quickCheck prop_append
  quickCheck prop_append_AVL
  qcAVL

