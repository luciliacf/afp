---
title: Exercise: Purely Functional Queues
date: October 11, 2017
---

> {-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
> 
> {-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
> 
> {-# OPTIONS_GHC -fdefer-type-errors #-}
> module Queue where

> import Test.QuickCheck
> 
> import qualified Data.Maybe as Maybe
> 

1. Define an interface for a purely functional Queue (FIFO) using a type
class. It must (at least) have a way to add an element to the end of the queue
(enqueue) and remove the element at the beginning of the queue (dequeue).

> 
> class Queue q where
>    empty  :: q a
>    enq    :: q a -> a -> q a
>    deq    :: q a -> Maybe (a, q a)

> fromList :: Queue q => [a] -> q a
> fromList = foldl enq empty

> toList :: Queue q => q a -> [a]
> toList q = case deq q of
>               Just (x,q') -> x : toList q'
>               Nothing     -> []

> 


2. Define some properties that your queue should satisfy.

> 
> prop_deq_empty :: forall q. (Queue q) => Bool
> prop_deq_empty = Maybe.isNothing (deq (empty :: q Int))

> prop_ordered :: forall q. Queue q => [Int] -> Bool
> prop_ordered xs = xs == toList (fromList xs :: q Int)
> 

3. Implement your interface.

> 
> data Q a  =  Q [a] [a] deriving (Eq, Show)

> instance Queue Q where
>   empty = Q [] []

>   enq (Q b f) x = Q (x:b) f

>   deq (Q [] [])    = Nothing
>   deq (Q b (x:xs)) = Just (x, Q b xs)
>   deq (Q b []    ) = deq (Q [] (reverse b))
> 

4. Make an arbitrary instance.

> 
> instance Arbitrary a => Arbitrary (Q a) where
>    arbitrary = Q <$> (listOf arbitrary) <*> (listOf arbitrary)
> 

5. Run your tests.

> 
> main :: IO ()
> main = do
>   quickCheck $ prop_deq_empty @Q
>   quickCheck $ prop_ordered @Q
> 
