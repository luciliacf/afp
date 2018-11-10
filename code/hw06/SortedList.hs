{-# OPTIONS  -fwarn-tabs -fwarn-incomplete-patterns -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SortedList ( SortedList,    -- the abstract type (and its instances)
                    singleton,     -- other functions
                    toNormalList,
                    minimum,
                    numDistinct,
                    count
                  ) where

import Test.HUnit
import Prelude hiding ( minimum, maxmimum )
import qualified Data.List as List
import Data.Coerce

import Data.Monoid

testListMonoid :: Test
testListMonoid =
  let t1 = [1,2] in
  let t2 = [3,4] in
  let t3 = [1,2,3,4] in
  TestList [ mempty <> t1     ~?= t1,              -- left identity
             t1 <> mempty     ~?= t1,              -- right identity
             (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
            ]

reduce :: Monoid b => [b] -> b
reduce = foldr (<>) mempty

ten :: Int
ten = getSum (reduce (map Sum [1,2,3,4]))

twentyFour :: Int
twentyFour = getProduct (reduce (map Product [1,2,3,4]))

---------------------------------------------------------

newtype SortedList a = SortedList [a] deriving (Eq, Show)

toNormalList :: SortedList a -> [a]
toNormalList (SortedList as) = as

singleton :: a -> SortedList a
singleton a = SortedList [a]

merge [] ys = ys
merge xs [] = xs
merge l1@(x:xs) l2@(y:ys)
   | x < y     = x : merge xs l2
   | x == y    = x : y : merge xs ys
   | otherwise = y : merge l1 ys 

instance Ord a => Semigroup (SortedList a) where
  (SortedList l1) <> (SortedList l2) = SortedList $ merge l1 l2

instance Ord a => Monoid (SortedList a) where
  mempty = SortedList []

testSortedList :: Test
testSortedList =
  let t1 = SortedList [2,4] in
  let t2 = SortedList [1,5] in
  let t3 = SortedList [2,3] in
  TestList [ t1 <> t3 ~?= SortedList [2,2,3,4],    -- <> preserves sorting
             mempty <> t1 ~?= t1,                  -- left identity
             t1 <> mempty ~?= t1,                  -- right identity
             (t1 <> t2) <> t3 ~?= t1 <> (t2 <> t3) -- associativity
           ]

---------------------------------------------------------

-- returns the minimum element of the SortedList
minimum :: SortedList a -> Maybe a
minimum (SortedList [])     = Nothing
minimum (SortedList (x:xs)) = Just x

testMinimum :: Test
testMinimum =
  let t1 = SortedList [1,3,5] in
  let t2 = SortedList ([] :: [Int]) in
  let t3 = SortedList [1, error "kaboom!"] <> SortedList[2] in
  TestList [ minimum t1 ~?= Just 1   -- the minimum of a non-empty sorted list
           , minimum t2 ~?= Nothing  -- the minimum of an empty sorted list
           , minimum t3 ~?= Just 1   -- minimum need not examine whole list
           ]

-- returns the number of distinct elements of the SortedList
numDistinct :: Ord a => SortedList a -> Int
numDistinct (SortedList [])        = 0
numDistinct (SortedList xs@(y:ys)) = 1 + (length $ filter (==False) $ zipWith (==) xs ys)  

testNumDistinct :: Test
testNumDistinct = TestList
  [numDistinct (SortedList ([1,1,3,3,5]::[Int])) ~?= 3,
   numDistinct (SortedList ([2,2,3,4,4,4,5,6,6,6,6]::[Int])) ~?= 5,
   numDistinct (SortedList ([]::[Int])) ~?= 0]

-- returns a list with the frequency count of each distinct element of the SortedList 
count :: Eq a => SortedList a -> [(a, Int)]
count (SortedList []) = []
count (SortedList (x:xs)) = (x, 1 + length ys) : count (SortedList zs)
  where (ys,zs) = span (==x) xs

testCount :: Test
testCount =
  let xs = SortedList "abbcccdddd" in
  count xs ~?= [('a', 1),('b',2),('c',3),('d',4)]
