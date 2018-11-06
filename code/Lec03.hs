{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Lec3 where
import Prelude hiding (map, foldr, filter, pred, sum, product)
import Data.Char
import Test.HUnit

-- plus1 and minus1
plus1 :: Int -> Int
plus1 x = x + 1

minus1 :: Int -> Int
minus1 x = x - 1

-- a pair of functions
funp :: (Int -> Int, Int -> Int)
funp = (plus1, minus1)

-- a list containing the functions
funs :: [Int -> Int]
funs = undefined  -- fill in a definition here

-- function as input
doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

dtTests :: Test
dtTests = TestList [ doTwice plus1  4 ~?= 6, doTwice minus1 5 ~?= 3 ]

-- returning a function 
plusn :: Int -> (Int -> Int)
plusn n = f
   where f x = x + n

plus10  :: Int -> Int
plus10  = plusn 10

minus20 :: Int -> Int
minus20 = plusn (-20)

-- partial application
plus :: Int -> Int -> Int
plus m n = m + n

plusfive :: Int -> Int
plusfive = plus 5

pfivetest :: Test
pfivetest = plusfive 1000 ~?= 1005

doTwicePlus20 :: Int -> Int
doTwicePlus20 = doTwice (plus 20)

-- anonymous Functions
anonTests :: Test
anonTests = TestList [ (\x -> x + 1) 100 ~?= 101, doTwice (\x -> x + 1) 100 ~?= 102 ]

plus1' :: Int -> Int
plus1' = \x -> x + 1

-- infix operations and sections
anotherFive :: Int
anotherFive = 2 `plus` 3

anotherFour :: Int
anotherFour = doTwice (+2) 0

singleton :: a -> [a]
singleton = undefined   -- fill in your definition

singletonTest :: Test
singletonTest = singleton True ~?= [True]

-- Polymorphism
greaterThan10 :: Int -> Bool
greaterThan10 = (10 <)

ex1 :: (a -> a) -> a -> a
ex1 = doTwice doTwice

ex1Test :: Test
ex1Test = undefined  -- fill in your definition

-- polymorphic Data Structures
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs

impossible :: a
impossible = undefined

-- Computation Pattern: Iteration 

toUpperString :: String -> String
toUpperString [] = []
toUpperString (x:xs) = toUpper x : toUpperString xs

-- polygon
type XY      = (Double, Double)
type Polygon = [XY]

shiftXY :: XY -> XY -> XY
shiftXY (dx, dy) (x, y) = (x+dx,y+dy)

shiftPoly :: XY -> Polygon -> Polygon
shiftPoly _ []       = []
shiftPoly d (xy:xys) = shiftXY d xy : shiftPoly d xys

-- map
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

toUpperString' :: String -> String
toUpperString' xs = map toUpper xs

shiftPoly' :: XY -> Polygon -> Polygon
shiftPoly' d = undefined

testUp = toUpperString' "abc" ~?= toUpperString "abc"
testSP = shiftPoly' (0.5,0.5) [(1,1),(2,2),(3,3)] ~?= shiftPoly (0.5,0.5) [(1,1),(2,2),(3,3)]
testMap = runTestTT $ TestList $ [ testUp, testSP ]

listIncr :: [Int] -> [Int]
listIncr []     = []
listIncr (x:xs) = (x+1) : listIncr xs

listIncr' :: [Int] -> [Int]
listIncr' = undefined

-- Computation Pattern: Folding
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + (sum xs)

product :: [Int] -> Int
product [] = 1
product (x:xs) = x * (product xs)

-- foldr
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f base []     = base
foldr f base (x:xs) = x `f` (foldr f base xs)

sum', product' :: [Int] -> Int
sum'     = foldr (+) 0
product' = foldr (*) 1

testSum = sum' [1,2,3] ~?= sum [1,2,3]
testProd = product' [1,2,3] ~?= product [1,2,3]
foldrTest = runTestTT $ TestList [ testSum, testProd  ]

-- len :: [a] -> Int
-- len []     = 0
-- len (x:xs) = 1 + len xs

len' :: [a] -> Int
len' = undefined    -- fill in your definition

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Int -> Int
factorial' n = undefined  -- fill in your definition

-- filter :: (a -> Bool) -> [a] -> [a]

testF1 = filter (>10) [1..20] ~?= [11..20]
testF2 = filter (\l -> sum l <= 42) [ [10,20], [50,50], [1..5] ] ~?= [[10,20],[1..5]]
filterTests :: Test
filterTests = TestList [ testF1, testF2 ]

filter pred = undefined  -- fill in your definition
runFilterTests = runTestTT filterTests
