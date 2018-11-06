module Hw05 where

import Prelude hiding ((*>))
import Control.Applicative  hiding ((*>))
import qualified Text.Read as Read
import Test.HUnit


-- Buiding an email

data Address = Address String deriving (Eq,Show)

data Body = Body String deriving (Eq,Show)
data Email = Email Address Address Body deriving (Eq,Show)

-- valid email adress
mkAddress :: String -> Maybe Address
mkAddress = undefined

-- valid email body
mkBody :: String -> Maybe Body
mkBody = undefined

-- build email
mkEmail :: String -> String -> String -> Maybe Email
mkEmail = undefined

testMkEmail = TestList
   [ mkEmail "maria@gmail.com" "ana@gmail.com" "ola" ~?=
       Just (Email (Address "maria@gmail.com") (Address "ana@gmail.com") (Body "ola")),
     mkEmail "ola" "ana@gmail.com" "ola" ~?= Nothing,
     mkEmail "maria@gmail.com" "ola" "ola" ~?= Nothing,
     mkEmail "maria@gmail.com" "ana@gmail.com" "" ~?= Nothing ]



-- Defining generic control structures

pair :: Applicative f => f a -> f b -> f (a,b)
pair = liftA2 (,)

testPair = TestList
   [ pair (Nothing::Maybe Int) (Just 1) ~?= Nothing,
     pair (Just (1::Int)) (Just 'a') ~?= Just (1,'a'),
     pair [1,2,3] [4,5] ~?= [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)],
     (getZipList $ pair (ZipList [1,2,3]) (ZipList [4,5])) ~?= [(1,4),(2,5)] ]


infixl 4 *>
(*>) :: Applicative f => f a -> f b -> f b
(*>) = undefined

testDiscard = TestList
   [ [1,2,3] *> ['a'] ~?= ['a'],
     (Just 'a') *> Nothing ~?= (Nothing :: Maybe Char),
     Nothing *> (Just 'a') ~?= Just 'a' ]

sequenceL :: Applicative f => [f a] -> f [a]
sequenceL = undefined

testSequenceL = TestList
  [ sequenceL [(Just 'a'), (Just 'b'), (Just 'c')] ~?= Just ['a','b','c'] ,
    sequenceL [(Just 'a'), Nothing, (Just 'c')] ~?= (Nothing :: Maybe [Char]),
    sequenceL ([[1],[2],[3]] :: [[Int]]) ~?= ([[1,2,3]] :: [[Int]]),
    sequenceL ([]::[[Int]]) ~?= ([[]]::[[Int]]),
    sequenceL ([[]]::[[Int]])  ~?= ([]::[[Int]]),
    sequenceL ([[],[]]::[[Int]]) ~?= ([]::[[Int]]) ]
  
mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA = undefined

readInt :: String -> Maybe Int
readInt = Read.readMaybe

testMapA = TestList
  [ mapA readInt ["123", "30"] ~?= (Just [123,30] :: Maybe [Int]),
    mapA readInt ["123","ab"] ~?= (Nothing :: Maybe [Int]),
    mapA (:[]) ["123","ab"] ~?= [["123","ab"]] ]

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA = undefined

testReplicateA = TestList
  [ replicateA 3 (Just 'a') ~?= Just "aaa",
    replicateA 2 ([2,3,4]::[Int]) ~?= ([[2,2,3,3,4,4]]::[[Int]]) ]

