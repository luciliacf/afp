---
title: Exercise: Using the State monad for Random Generation
date: November 1, 2017
---

[STRanlhs]: STRandom.lhs
[STRan-sol]: STRandom-sol.html

*Note:* You should download the [lhs version][STRanlhs] of this module and
replace all parts marked undefined. Eventually, the [complete version][STRan-sol] will
be made available.


> module RandomGen where

> import System.Random (StdGen, mkStdGen, next, randomIO)
> import State
> import Control.Monad


Random Generation
-----------------

QuickCheck needs to randomly generate values of any type. It turns
out that we can use the state monad to define something like the `Gen` monad
used in the QuickCheck libary.

First, a brief discussion of pseudo-random number generators. [Pseudo-random
number generators](http://en.wikipedia.org/wiki/Pseudorandom_number_generator)
aren\'t really random, they just look like it. They are more like functions
that are so complicated that they might as well be random. The nice property
about them is that they are repeatable, if you give them the same *seed* they
will produce the same sequence of \"random\" numbers.

Haskell has a library for Pseudo-Random numbers called
[System.Random](http://hackage.haskell.org/packages/archive/random/latest/doc/html/System-Random.html).

~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type StdGen  -- a type for a "standard" random number generator.

-- | Construct a generator from a given seed. Distinct arguments
-- are likely to produce distinct generators.
mkStdGen :: Int -> StdGen

-- | Returns an Int that is uniformly distributed in a range of at least 30 bits.
next     :: StdGen -> (Int, StdGen)
~~~~~~~~~~~~~~~~~~~~~~~~~~

For example, we can generate a random integer by constructing a random
number generator, calling `next` and then projecting the result.

> testRandom :: Int -> Int
> testRandom i = fst (next (mkStdGen i))

If we\'d like to constrain that integer to a specific range (0,n) we
can use `nextBounded`. See if you can define that operation.

> nextBounded :: Int -> StdGen -> (Int, StdGen)
> nextBounded bound s = undefined

> -- test `nextBounded` with 20 randomly generated numbers, using the bound n
> testBounded :: Int -> Bool
> testBounded n = all (\x -> x >= 0 && x < n) (g n <$> [0 .. 20]) where
>     g x = fst . nextBounded x . mkStdGen


QuickCheck is defined by class types that can construct random
values. Let\'s do it first the hard way \.\.\. i.e\. by explicitly passing around the
state of the random number generator


> -- | Extract random values of any type
> class Arb1 a where
>    arb1 :: StdGen -> (a, StdGen)

> instance Arb1 Int where
>    arb1 = next

> instance Arb1 Bool where
>   arb1 = undefined

> testArb1 :: Arb1 a => Int -> a
> testArb1 = fst . arb1 . mkStdGen

What about for pairs?

> instance (Arb1 a, Arb1 b) => Arb1 (a,b) where
>   arb1 = undefined

NOTE: make sure that you don\'t always generate the same number for
a pair of Ints. If you get this behavior, you have a bug:


~~~~~~~{.haskell}
     ghci> testArb1 20 :: (Int, Int)
     (799602,799602)

     ghci> testArb1 2021 :: (Int, Int)
     (80867616,80867616)
~~~~~~~~~~~~

And for lists?

> instance (Arb1 a) => Arb1 [a] where
>   arb1 s = undefined


~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
     ghci> testArb1 2 :: [Int]
     [1000038160,1595063297]
     ghci> testArb1 3 :: [Int]
     []

     ghci> testArb1 4 :: [Int]
     [2054794989,327347192,267611348,863499775]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Ouch, there\'s a lot of state passing going on here.


State Monad to the Rescue
-------------------------

So, we have developed a reusable library for the State monad. Let\'s
use it to *define* a generator monad for QuickCheck.

Our reusable library [`State`](State.html) defines an abstract type for the State monad,
and the following operations for working with these sorts of computations.

~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type State s a = ...

instance Monad (State s) where ...

get      :: State s s
put      :: s -> State s ()

state    :: (s -> (a,s)) -> State s a
runState :: State s a -> s -> (a,s)

~~~~~~~~~~~~~~~~~~~~~~~~~~

Now let\'s define a type for generators, using the State monad.

> type Gen a = State StdGen a

With this type, we can create a type class similar to the one in the
QuickCheck library.

> class Arb a where
>   arb :: Gen a

For example, we can use the `state` operation to inject the `next`
function into the `State StdGen a` type.

> instance Arb Int where
>   arb = state next

What if we want a bounded generator?

> bounded :: Int -> Gen Int
> bounded b = undefined

Now define a `sample` function, which generates and prints 10 random values.

> sample :: Show a => Gen a -> IO ()
> sample gen = do
>   seed <- (randomIO :: IO Int) -- get a seed from the global random number generator
>                                -- hidden in the IO monad
>   undefined

For example, you should be able to sample using the `bounded` combinator.

~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
     ghci> sample (bounded 10)
     5
     9
     0
     5
     4
     6
     0
     0
     7
     6
~~~~~~~~~~~~~~~~~~~~~

What about random generation for other types?  How does the state
monad help that definition? How does it compare to the version above?

> instance (Arb a, Arb b) => Arb (a,b) where
>  arb = undefined

Can we define some standard quickcheck combinators to help us?
What about `elements`, useful for the `Bool` instance ?

> elements :: [a] -> Gen a
> elements = undefined

> instance Arb Bool where
>   arb = elements [False, True]

or `frequency`, which we can use for the `[a]` instance ?

> frequency :: [(Int, Gen a)] -> Gen a
> frequency = undefined

> instance (Arb a) => Arb [a] where
>   arb = frequency [(1, return []), (3, (:) <$> arb <*> arb)]

Of course, QuickCheck does a lot more than this, as you can tell from
sampling.  In particular, QC keeps track of more information, such as size
controls, during repeated uses of `arbitrary`. Furthermore, QC also stores all
of its state inside the `IO` monad instead of using the `State`
monad. However, this exercise should have given you more practice with the
state monad, as well as a better understanding of what is going on under the
hood with quickcheck.