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
reduce = foldr mappend mempty

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

instance Ord a => Semigroup (SortedList a) where
  (SortedList l1) <> (SortedList l2) = undefined

instance Ord a => Monoid (SortedList a) where
  mempty = undefined

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

minimum :: SortedList a -> Maybe a
minimum = undefined

testMinimum :: Test
testMinimum =
  let t1 = SortedList [1,3,5] in
  let t2 = SortedList ([] :: [Int]) in
  let t3 = SortedList [1, error "kaboom!"] <> SortedList[2] in
  TestList [ minimum t1 ~?= Just 1   -- the minimum of a non-empty sorted list
           , minimum t2 ~?= Nothing  -- the minimum of an empty sorted list
           , minimum t3 ~?= Just 1   -- minimum need not examine whole list
           ]

numDistinct :: Ord a => SortedList a -> Int
numDistinct = undefined

testNumDistinct :: Test
testNumDistinct = TestList
  [numDistinct (SortedList ([1,1,3,3,5]::[Int])) ~?= 3,
   numDistinct (SortedList ([2,2,3,4,4,4,5,6,6,6,6]::[Int])) ~?= 5,
   numDistinct (SortedList ([]::[Int])) ~?= 0]

count :: Eq a => SortedList a -> [(a, Int)]
count = undefined

testCount :: Test
testCount =
  let xs = SortedList "abbcccdddd" in
  count xs ~?= [('a', 1),('b',2),('c',3),('d',4)]

