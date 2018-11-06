{-# OPTIONS -fwarn-tabs -fno-warn-type-defaults #-}

module Lec2 where
import Data.Char
import Test.HUnit

-- Tuples
t1 = ('a', 5)          :: (Char, Int)             -- the spacing doesn't matter
t2 = ('a', 5.2, 7)     :: (Char, Double, Int)     -- but it is pretty to line
t3 = ((7, 5.2), True)  :: ((Int, Double), Bool)   -- things up in your code

pat :: (Int, Int, Int) -> Int
pat (x, y, z) = x * (y + z)

tup1 = ((1,2),3) :: ((Int,Int),Int)  -- a pair of a pair and a number
tup2 = (1,(2,3)) :: (Int,(Int,Int))  -- a pair of a number and a pair
tup3 = (1, 2, 3) :: (Int, Int, Int)  -- a three-tuple

pat2 :: ((Int,Int),Int) -> Int
pat2 ((x, y), z) = x * (y + z)

pat3 :: (Int, (Int, Int)) -> Int
pat3 (x, (y, z)) = x * (y + z)

act2 :: (IO (), IO ())
act2 = (putStr "Hello ", putStr "World ")

runAct2 :: IO ()
runAct2 = do
   let (x, y) = act2     -- pattern match in `do` sequences using `let`
   x                     -- run the first action
   y                     -- then run the second

runAct2' :: IO ()
runAct2' = do
   let (x, y) = act2     -- pattern match
   y                     -- run the second action
   x                     -- then run the first

runAct2'' :: IO ()
runAct2'' = do
    let (x, y) = act2   -- pattern match
    x                   -- run the first action
    x                   -- then run it again!


-- Maybe a

m1 :: Maybe Int
m1 = Just 2

m2 :: Maybe Int
m2 = Nothing

pat'' :: Maybe Int -> Int
pat'' (Just x) = x
pat'' Nothing  = 2

jn :: Maybe (Maybe a) -> Maybe a
jn (Just (Just x)) = Just x
jn (Just Nothing) = Nothing
jn Nothing = Nothing

jn' :: Maybe (Maybe a) -> Maybe a
jn' = undefined

location :: String -> Maybe String
location "cis501" = Just "Wu & Chen"
location "cis502" = Just "Heilmeier"
location "cis520" = Just "Wu & Chen"
location "cis552" = Just "3401 Walnut: 401B"
location _        = Nothing    -- wildcard pattern, matches anything

-- Lists

l1 :: [Double]
l1 = [1.0,2.0,3.0,4.0]

l2 :: [Int]
l2 = undefined -- make a list of numbers

l3 :: [(Int,Bool)]
l3 = [ (1,True), (2, False) ]

l4 :: [[Int]]
l4 = undefined

-- l5 :: [Int]
-- l5 = [ 1 , True ]  -- doesn't type check

l6 :: [a]
l6 = []

l7 :: String
l7 = ['h','e','l','l','o',' ','5','5','2','!']

cons :: a -> [a] -> [a]
cons = (:)

c1 :: [Char]
c1 = 'a' : ['b', 'c']

c2 :: [Int]
c2 = 1 : []

-- undefined: fill in the type of c3
c3 = [] : []

-- Example: clone
testClone1, testClone2, testClone3 :: Test
testClone1 = clone 'a' 4 ~?= ['a','a','a','a']
testClone2 = clone 'a' 0 ~?= []
testClone3 = clone 1.1 3 ~?= [1.1, 1.1, 1.1]
testClone4 = clone 'a' (-1) ~?= []

clone :: a -> Int -> [a]
clone x n = if n<=0 then [] else x : clone x (n-1)

cl1, cl2, cl3 :: IO Counts
cl1 = runTestTT testClone1
cl2 = runTestTT testClone2
cl3 = runTestTT testClone3
cl4 = runTestTT testClone4

cls :: IO Counts
cls = runTestTT (TestList [ testClone1, testClone2, testClone3, testClone4 ])

-- Function practice
-- Exercise 2.2  -- range

testRange :: Test
testRange = TestList [ range 3  6 ~?= [3,4,5,6], range 42 42 ~?= [42], range 10 5  ~?= [] ]

range :: Int -> Int -> [Int]
range i j = undefined   -- fill in your definition

runRTests :: IO Counts
runRTests = runTestTT testRange

-- Pattern matching with lists

isHi :: String -> Bool
isHi ['H','i'] = True
isHi _ = False

isGreeting :: String -> Bool
isGreeting "Hi" = True
isGreeting "Hello" = True
isGreeting "Bonjour" = True
isGreeting "Guten Tag" = True
isGreeting _ = False

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _   = False


-- test if a list is longer than two 
isLong :: [a] -> Bool
isLong = undefined      -- fill in you definition

testIsLong :: Test
testIsLong = TestList [ not (isLong [])   ~? "nil", -- can convert booleans to tests by naming them via `~?`
                        not (isLong "a")  ~? "one",
                        not (isLong "ab") ~? "two",
                        isLong "abc"      ~? "three" ]

runLTests :: IO Counts
runLTests = runTestTT testIsLong

-- Function practice: List Recursion
-- Exercise 2.3 - listAdd

listAddTests :: Test
listAddTests = TestList [ listAdd [1,2,3] ~?= 6, listAdd [] ~?= 0 ]

listAdd :: [Int] -> Int
listAdd = undefined  -- fill in your definition

runLATests :: IO Counts
runLATests = runTestTT listAddTests

-- Function practice: List transformation
-- Exercise 2.4 -  listincr

listIncrTests :: Test
listIncrTests = TestList [ listIncr [1,2,3] ~?= [2,3,4], listIncr [42] ~?= [43] ]

listIncr :: [Int] -> [Int]
listIncr = undefined  -- fill in your definition

runLITests :: IO Counts
runLITests = runTestTT listIncrTests