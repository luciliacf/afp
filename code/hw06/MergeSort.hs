module MergeSort where

import SortedList
import Data.Monoid
import Test.HUnit

insert :: Ord a => a -> [a] -> [a]
insert y [] = [y]
insert y l@(x:xs)
   | y <= x    = y : l
   | otherwise = x : insert y xs  

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

-------------------------------------------------------------

sortedFromList :: Ord a => [a] -> SortedList a
sortedFromList []     = mempty
sortedFromList (x:xs) = singleton x <> sortedFromList xs 

sortedListSort :: Ord a => [a] -> [a]
sortedListSort = toNormalList . sortedFromList

testSortedFromList :: Test
testSortedFromList = 
  let unsorted = [51,67,89,95,14,31,28,87,0,25]
      sorted   = [0,14,25,28,31,51,67,87,89,95] in
  sortedListSort unsorted ~?= sorted

sortedFromList' :: Ord a => [a] -> SortedList a
sortedFromList' = foldMapList singleton

sortedListSort' :: Ord a => [a] -> [a]
sortedListSort' = toNormalList . sortedFromList'

testSortedFromList' :: Test
testSortedFromList' =
  let unsorted = [47,80,28,47,45,76,1,35,19,1] in
  sortedListSort' unsorted ~?= sortedListSort unsorted  -- old & new agree

foldMapList :: Monoid m => (a -> m) -> [a] -> m
foldMapList f = foldr (mappend) mempty . fmap f 

sumOfProducts :: Num a => [[a]] -> a
sumOfProducts = getSum . foldMapList (Sum . getProduct . foldMap (Product))  

testSumOfProducts :: Test
testSumOfProducts = sumOfProducts [[1],[2,3],[4,5,6],[7,8,9,10]] ~?= 5167

data Crispy a = Snap a [a] a
              | Crackle [[Crispy a]]
              | Pop Integer deriving (Eq,Show)

instance Functor Crispy where
   -- fmap :: (a -> b) -> m a -> m b
   fmap f (Snap x xs y)  = Snap (f x) (fmap f xs) (f y)
   fmap f (Crackle xss ) = Crackle (fmap (fmap (fmap f)) xss)
   fmap f (Pop n)        = Pop n 

instance Foldable Crispy where
   -- foldMap :: (Foldable t , Monoid m) => (a -> m) -> t a -> m
   foldMap f (Snap x xs y) = f x <> foldMap f xs <> f y
   foldMap f (Crackle xss) = foldMap (foldMap (foldMap f)) xss
   foldMap f (Pop n)       = mempty

testCrispy :: Test
testCrispy =
  let c1, c2, c3, c4, c5 :: Crispy Integer
      c1 = fmap (+1) (Snap 0 [1,2,3] 4)
      c2 = Snap 700 [] 600
      c3 = Pop 1234567890
      c4 = Crackle [[c3, c1], [c3, c1]]
      c5 = fmap (subtract 1) (Crackle [[c1, c2], [c1, c3]]) in
  TestList [ 15 ~?= getSum (foldMap Sum c1)
           , 1 ~?= getProduct (foldMap Product c3)
           , "0123469959901234" ~?= foldMap show c5]

-------------------------------------------------------------

newtype DivideList a = DivideList { getDivideList :: [a] } deriving (Eq, Show)

instance Semigroup (DivideList a) where
   (DivideList xs) <> (DivideList ys) = DivideList $ xs <> ys

instance Monoid (DivideList a) where
  mempty = DivideList []

divide :: DivideList a -> (DivideList a, DivideList a)
divide (DivideList xs) = (DivideList ys, DivideList zs)
  where (ys,zs) = splitAt (length xs `div` 2) xs
          
testDivide :: Test
testDivide = TestList [ divide (DivideList "abcd") ~?=
                       (DivideList "ab", DivideList "cd"),
                      divide (DivideList "abcde") ~?=
                       (DivideList "ab", DivideList "cde"),
                      divide (DivideList "") ~?=
                       (DivideList "", DivideList "") ]

instance Foldable DivideList where
  -- foldMap :: (Foldable t , Monoid m) => (a -> m) -> t a -> m
  foldMap _ (DivideList [])  = mempty
  foldMap f (DivideList [x]) = f x 
  foldMap f xs = foldMap f ys <> foldMap f zs
     where (ys,zs) = divide xs

testDivideList :: Test
testDivideList =
  let xs = DivideList [1,2,3]
      ys = DivideList [] in
  TestList [ Product 6 ~?= foldMap Product xs
           , Sum 0     ~?= foldMap Sum ys
           ]

foldSort :: (Ord a, Foldable t) => t a -> [a]
foldSort = toNormalList . foldMap singleton

realMergeSort :: Ord a => [a] -> [a]
realMergeSort = foldSort . DivideList

main :: IO ()
main = (print . last . realMergeSort) [100000,99999..0]
