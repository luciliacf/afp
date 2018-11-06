---
title: Using Applicative
date: September 2018
---

[hw05]: ../homework/Hw05.hs
[hw05-sol]: ../code/Hw05.zip
[cwebpage]: 

Edit the file [Hw05.hs][hw05] for this problem.
A solution to this homework will eventually be available [here][hw05-sol]

> module Hw05 where
>
> import Control.Applicative hiding ((*>))
> import qualified Text.Read as Read
> import Test.HUnit

Typeclass `Applicative`, defined in module `Data.Applicative`, abstracts a generalised form of
function application for which the argument function, the argument value and the result value
are all contained in `f` structures

     class Functor m ⇒ Applicative m where
       pure  :: a → m a
       (<*>) :: m (a → b) → m a → m b

Let\'s see a simple example where this allows to write simpler and more clear code.

A simple example: Buiding an email
========================================

Consider an email is composed by a sender address, a receiver address and a body, as defined by the following
type declarations:

> data Address = Address String           deriving (Eq,Show)
> data Body = Body String                 deriving (Eq,Show)
> data Email = Email Address Address Body deriving (Eq,Show)

Your task is to write a function `mkEmail :: FromAdress -> ToAdress -> Body -> Maybe Email`, that builds an email, if its
arguments are two valid `Adress` values and a valid `Body`. A valid adress is simply a string containg the character `@`,
and valid body is a non empty string. First define the following auxiliary functions: 

> -- valid email adress
> mkAddress :: String -> Maybe Address
> mkAddress = undefined 
>
>-- valid email body
> mkBody :: String -> Maybe Body
> mkBody = undefined 

You will now define function `mkEmail`. A first attempt, without using the applicative style, might be:

     mkEmail' :: String -> String -> String -> Maybe Email 
     mkEmail' from to body =
        case mkAddress from of
          Nothing -> Nothing
          Just fromAddress -> case mkAddress to of
                                Nothing -> Nothing
                                Just toAddress -> case mkBody body of
                                                    Nothing -> Nothing
                                                    Just bodyOk -> Just (Email fromAddress toAddress bodyOk)

But this looks too repetitive and ugly! Let\'s try to define `mkEmail` using applicative style:

> -- build email
> mkEmail :: String -> String -> String -> Maybe Email
> mkEmail = undefined 


> testMkEmail = TestList
>    [ mkEmail "maria@gmail.com" "ana@gmail.com" "ola" ~?=
>        Just (Email (Address "maria@gmail.com") (Address "ana@gmail.com") (Body "ola")),
>      mkEmail "ola" "ana@gmail.com" "ola" ~?= Nothing,
>      mkEmail "maria@gmail.com" "ola" "ola" ~?= Nothing,
>      mkEmail "maria@gmail.com" "ana@gmail.com" "" ~?= Nothing ]


Defining generic control structures
-----------------------------------

One of the benefits of having a unified interface like `Applicative` is that we can write generic tools and
control structures that work with any type which is an instance of `Applicative`. As a first example,
let\'s try writing function `pair`, which takes two values and pairs them, but all in the context of some `Applicative f`:

> pair :: Applicative f => f a -> f b -> f (a,b)

As a first try we can take a function for pairing and \“lift\” it over the arguments using (`<$>`) and (`<*>`):

~~~~~{.haskell}
pair fa fb = (\x y -> (x,y)) <$> fa <*> fb
~~~~~~~~~

This works, though we can simplify it a bit. First, note that Haskell allows the special syntax `(,)` to represent
the pair constructor, so we can write

~~~~~{.haskell}
pair fa fb = (,) <$> fa <*> fb
~~~~~~~~~

But actually, we can further simplify this by using `liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c`,
which lifts a 2-argument function to operate in the context of some `Applicative`.

~~~~~{.haskell}
pair fa fb = liftA2 (,) fa fb
~~~~~~~~~~~~

but now there is no need to explicitly write out the function arguments, so we reach our final simplified version:

> pair = liftA2 (,)

Now, what does this function do? It depends, of course, on the particular `f` chosen. Let\'s consider a number of
particular examples:

  * `f = Maybe`: the result is `Nothing` if either of the arguments is; if both are Just the result is `Just` their pairing.
  * `f = []`: pair computes the Cartesian product of two lists.
  * `f = ZipList`: pair is the same as the standard `zip` function.
 
> testPair = TestList
>    [ pair (Nothing::Maybe Int) (Just 1) ~?= Nothing,
>      pair (Just (1::Int)) (Just 'a') ~?= Just (1,'a'),
>      pair [1,2,3] [4,5] ~?= [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)],
>      (getZipList $ pair (ZipList [1,2,3]) (ZipList [4,5])) ~?= [(1,4),(2,5)] ]

Your task is to define the following generic functions, using the `Applicative`interface..

The binarry infix operator `*>` takes two effectful computations of types and sequeces
these conputations, discarding the value of the first argument. (*Hint*: Use function
`const :: a -> b -> a`, defined by `const x _ =  x`).

> infixl 4
> (*>) :: Applicative f => f a -> f b -> f b
> (*>) = undefined
>
>testDiscard = TestList
>   [ [1,2,3] *> ['a'] ~?= ['a'],
>     (Just 'a') *> Nothing ~?= (Nothing :: Maybe Char),
>     Nothing *> (Just 'a') ~?= Just 'a' ]

Function `sequenceL` transforms a list of applicative actions into a single such action that returns a list of
result values (We have already seen this function in Lecture 6, but try to figure out a definition by yorself):

> sequenceA :: Applicative f => [f a] -> f [a]
> sequenceA = undefined
>
>testSequenceL = TestList
>  [ sequenceL [(Just 'a'), (Just 'b'), (Just 'c')] ~?= Just ['a','b','c'] ,
>    sequenceL [(Just 'a'), Nothing, (Just 'c')] ~?= (Nothing :: Maybe [Char]),
>    sequenceL ([[1],[2],[3]] :: [[Int]]) ~?= ([[1,2,3]] :: [[Int]]),
>    sequenceL ([]::[[Int]]) ~?= ([[]]::[[Int]]),
>    sequenceL ([[]]::[[Int]])  ~?= ([]::[[Int]]),
>    sequenceL ([[],[]]::[[Int]]) ~?= ([]::[[Int]]) ]

Function `mapA` maps a function from type `a` into an applicative action of type `f b` over a list of elements
of type `a` and returns a single `f` action containg the list of resuls.
(*Hint*: you may use function `sequenceL` in the definition of `mapA`):    

> mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
> mapA = undefined
>
>readInt :: String -> Maybe Int
>readInt = Read.readMaybe
>
>testMapA = TestList
>  [ mapA readInt ["123", "30"] ~?= (Just [123,30] :: Maybe [Int]),
>    mapA readInt ["123","ab"] ~?= (Nothing :: Maybe [Int]),
>    mapA (:[]) ["123","ab"] ~?= [["123","ab"]] ]

Function `replicateA` takes an `Int` value `n` and an applicative structure of type `f a` and returns a single
`f` structure containing a list with `n` repetitions of the value contained in the original `f` structure.

> replicateA :: Applicative f => Int -> f a -> f [a]
> replicateA = undefined
>
> testReplicateA = TestList
>   [ replicateA 3 (Just 'a') ~?= Just "aaa",
>     replicateA 2 ([2,3,4]::[Int]) ~?= ([[2,2,3,3,4,4]]::[[Int]]) ]


