---
title: The State Monad!
date: Oct 30, 2017
---

[RWH]: http://book.realworldhaskell.org/
[stlhs]: StateMonad.lhs
[st-sol]: StateMonad-sol.html

*Note:* You should download the [lhs version][stlhs]
of this module and replace all parts marked undefined. Eventually, the [complete version][st-sol] will
be made available.

Set-up
------

In this lecture, we\'ll continue our study of monads via examples of
*specific* monads to try to understand how they work. At this point,
don\'t panic if you don\'t understand the big picture: each of the
specific instances is useful in its own right. .

> {-# LANGUAGE InstanceSigs #-}
> module Monads2 where
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Control.Monad (liftM, ap)

This module depends on an auxiliary module that you will define later.

> import State

Note: to load this module into ghci, you must first change into the
current subdirectory so that ghci can find it.  

Alternatively, the `State` module is a subset of the functionality
in the standard library. We can also replace the import above with:

> -- import Control.Monad.State


The State Monad
---------------

Now let us consider the problem of writing functions that manipulate
some kind of state. We\'re going to start with some examples of state
manipulation, written in an awkward style, and then show how monads
can cleanly abstract the sequencing necessary for such programs.

By way of an example of state manipulation, let\'s go back to binary
trees whose leaves contains values of some type `a`:

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>   deriving (Eq, Show)

Here is a simple example:

> tree :: Tree Char
> tree =  Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')

A functional programmer would count the number of leaves in a tree
like so:

> countF :: Tree a -> Int
> countF (Leaf _) = 1
> countF (Branch t1 t2) = countF t1 + countF t2

On the other hand, consider how a C programmer would count the number of
leaves in a tree. She might create a local variable and then then walk
the tree, incrementing the variable at each leaf.

In pure code, we *cannot* modify the values of any variables. However, we can
emulate this pattern with a *store transformer* \-\- a function that takes an
initial state as an input and returns the new state at every step.

In this example, the state, also called the \"store\", is an `Int` (representing
the current count) and a store transformer is a function of type `Int ->
Int`. See if you can implement an appropriate store transformer.

> -- The number of leaves in the tree that we have currently counted
> type Store = Int

> countI :: Tree a -> Int
> countI t = aux t 0 where
>   aux :: Tree a -> (Store -> Store)
>   aux = undefined

Once you have completed the implementation, test it on the sample tree
above.

       ghci> countI tree
       3

In general, a store transformer takes a current store as its argument, and
produces a modified store as its result, where the modified store reflects any
side effects performed by the function.

Next consider the problem of defining a function that labels each leaf
in such a tree with a unique or \"fresh\" integer.  This can be achieved
by taking the next fresh integer as an additional argument to a helper
function, and returning the next fresh integer as an additional
result.

> label1 :: Tree a -> Tree (a, Int)
> label1 t = fst (aux t 0) where
>    aux :: Tree a -> Store -> (Tree (a,Int), Store)
>    aux = undefined

Once you have completed the implementation, again test it on the sample tree
above.

        ghci> label1 tree
        Branch (Branch (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2))

     SPOILER SPACE BELOW

      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |
      |


Here\'s my version:

> label1' :: Tree a -> Tree (a, Int)
> label1' t = fst (aux t 0) where
>    aux :: Tree a -> Store -> (Tree(a,Int), Store)
>    aux (Leaf x)       s = (Leaf (x,s), s+1)
>    aux (Branch t1 t2) s = let (t1',s')  = aux t1 s
>                               (t2',s'') = aux t2 s'
>                           in (Branch t1' t2', s'')

This example demonstrates that in general, we may wish to return a
result value in addition to updating the store. For this reason, we
generalize our type of store transformers to also return a result
value, with the type of such values being a parameter of the `ST`
type:

> type ST a = Store -> (a, Store)

The store transformer may also wish to take argument values.  However,
there is no need to further generalize the `ST` type to take account
of this, because this behavior can already be achieved by _currying_
(i.e., treating multi-argument functions as functions returning
functions).  For example, the store transformer for the tree above
takes a tree and returns a labeled tree and has type `Tree a -> ST
(Tree (a,Int))`, which abbreviates the curried function type

~~~~~{.haskell}
Tree a -> Store -> (Tree (a, Int), Store)
~~~~~

The reason we are talking about store transformers is that parameterized type
`ST` is a *monad*.  What are its definitions of `return` and `bind`? If you
get stuck, try expanding the definitions of `ST a` in the types below and see
where that leads

> returnST :: a -> ST a
> returnST = undefined

> bindST :: ST a -> (a -> ST b) -> ST b
> bindST = undefined

That is, `returnST` converts a value into a store transformer that simply
returns that value without modifying the state.

In turn, `bindST` provides a means of sequencing store transformers: `bindST
st f` applies the store transformer `st` to an initial state `s`, then applies
the function `f` to the resulting value `x` to give a second store transformer
`(f x)`, which is then applied to the modified store `s'` to give the final
result.

Now see if you can rewrite this slight modification to `label1` above. (We
have changed the type annotation for `aux` and moved the `s` argument to the
RHS.) Try to replace the RHS of `aux (Branch t1 t2)` with applications of
`bindST` and `returnST`.  (Don\'t try to do the same with the `Leaf`, we\'ll
need something else for this case.)

> label2 :: Tree a -> Tree (a, Int)
> label2 t = fst (aux t 0) where
>   aux ::  Tree a -> ST (Tree (a,Int))
>   aux (Leaf x)       = \s -> (Leaf (x,s), s+1)
>   aux (Branch t1 t2) = \s -> let (t1', s')  = aux t1 s in
>                              let (t2', s'') = aux t2 s' in
>                              (Branch t1' t2', s'')


We conclude this section with two technicalities.

1. We would like to just say:

~~~~~{.haskell}
type ST a = Store -> (a, Store)

instance Monad ST where
   -- return :: a -> ST a
   return    = returnST

   -- (>>=)  :: ST a -> (a -> ST b) -> ST b
   st >>= f  = bindST st f
~~~~~


   However, in Haskell, types defined using the `type` mechanism cannot be
   made into instances of classes.  Hence, in order to make ST into an
   instance of the class of monadic types, in reality it needs to be redefined
   using the \"data\" (or `newtype`) mechanism, which requires introducing a
   dummy constructor (called `S` for brevity):

   It is convenient to define our own application function for this type,
   which simply removes the dummy constructor. We do so using a _record
   type_ declaration, which lets us name the arguments to a data
   constructor.

> newtype ST2 a = S { apply :: Store -> (a, Store) }

~~~~~~~~~~~~~~~{.haskell}
ghci> :type S
S     :: (Store -> (a,Store)) -> ST2 a

ghci> :type apply
apply :: ST2 a -> Store -> (a, Store)
~~~~~~~~~~~~~~~

   (Records in Haskell are pretty similar to most other languages, but
   with a few convenient twists.  Chapter 3 of [RWH][RWH] gives more details.)

   `ST2` is now defined as a monadic type (an instance of the `Monad`
    class) as follows:

> instance Monad ST2 where
>   return :: a -> ST2 a
>   return x   = S $ \s -> (x,s)

>   (>>=)  :: ST2 a -> (a -> ST2 b) -> ST2 b
>   f >>= g   = S $ \s -> let (a, s')  = apply f s
>                          in apply (g a) s'

   (*Aside*: there is no runtime overhead for manipulating the dummy
   constructor because we defined `ST2` using the `newtype` mechanism of
   Haskell, rather than `data`.)

2. Starting with GHC 7.10, all monads in Haskell must also be applicative
   functors. However, we can declare these instances using `ap` and `liftM`
   which are defined in `Control.Monad.`

> instance Functor ST2 where
>   fmap  = liftM

> instance Applicative ST2 where
>   pure  = return
>   (<*>) = ap


Now, let\'s rewrite the tree labeling function with the `ST2`
monad.

In order to generate a fresh integer, we define a special state
transformer that simply returns the current state as its result, and
the next integer as the new state:

> fresh :: ST2 Int
> fresh = undefined

This function should transform the store as follows: when given an initial
store, it should return that store paired with the incremented store.

     ghci> apply fresh 1
     (1,2)

This function is another useful operation for the `ST2` type besides `bind` and
`return`. (The fact that `ST2` is a monad is not the *only* important property of
this type.)

Using this function, together with the `return` and `>>=` primitives that are
provided by virtue of `ST2` being a monadic type, it is now straightforward to
define a function that takes a tree as its argument, and returns a store
transformer that produces the same tree with each leaf labelled by a fresh
integer:

> mlabel            :: Tree a -> ST2 (Tree (a,Int))
> mlable (Leaf x) = undefined
> mlabel (Branch t1 t2) = undefined

Try to write `mlabel` both with and without `do`-notation.

Note that in either version, the programmer does not have to worry about the
tedious and error-prone task of dealing with the plumbing of fresh labels, as
this is handled automatically by the state monad.

Finally, we can now define a function that labels a tree by
simply applying the resulting store transformer with zero as
the initial state, and then discarding the final state:

> label  :: Tree a -> Tree (a, Int)
> label t = undefined

For example, `label tree` gives the following result:

~~~~~{.haskell}
ghci> label tree
Branch (Branch (Leaf ('a', 0)) (Leaf ('b',1))) (Leaf ('c', 2))
~~~~~

A Generic Store Transformer
===========================

Often, the *store* that we want to have will have multiple components
\-\- e.g., multiple variables whose values we might want to update. This
is easily accomplished by using a different type for `Store` above,
for example, if we want two integers, we might use the definition

~~~~~{.haskell}
type Store = (Int, Int)
~~~~~

and so on.

However, we would like to write reusable code, which will work with
any store.

The file [State](State.html) ( [lhs version](State.lhs) ) contains a generic
library for that purpose. You should switch to that file now and read it
before moving on. The code below will *use* those definitions.



Using a Generic Store Transformer
=================================

Let us use our generic state monad to rewrite the tree labeling function
from above. Note that the actual type definition of the generic transformer
is *hidden* from us, so we must use only the publicly exported functions:
`get`, `put` and `runState` (in addition to the monadic functions we get for
free.)


First, we write an action that returns the next fresh integer. (Note
that the first type argument is the store, while the second is the
result type of the monadic action.)

> freshS :: State Int Int
> freshS = undefined

Now, the labeling function with our generic `State` monad is straightforward.

> mlabelS :: Tree t -> State Int (Tree (t, Int))
> mlabelS (Leaf x)     = do y <- freshS
>                           return (Leaf (x, y))
> mlabelS (Branch t1 t2) = do t1' <- mlabelS t1
>                             t2' <- mlabelS t2
>                             return (Branch t1' t2')


Easy enough!

~~~~~{.haskell}
ghci> runState (mlabelS tree) 0
(Branch (Branch (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2)),3)
~~~~~

We can *execute* the action from any initial state of our choice

~~~~~{.haskell}
ghci> runState (mlabelS tree) 1000
(Branch (Branch (Leaf ('a',1000)) (Leaf ('b',1001))) (Leaf ('c',1002)),1003)
~~~~~

Now, what\'s the point of a generic store transformer if we can\'t have richer
states? Next, let us extend our `fresh` and `label` functions so that

- each node gets a new label (as before), and

- the state also contains a map of the *frequency* with which each
  leaf value appears in the tree.

Thus, our state will now have two elements, an integer denoting the *next*
fresh integer, and a `Map a Int` denoting the number of times each leaf
value appears in the tree. (Documentation for the
[Data.Map module](http://www.haskell.org/ghc/docs/latest/html/libraries/containers/Data-Map.html). )

> data MySt a = M { index :: Int
>                 , freq  :: Map a Int }
>               deriving (Eq, Show)

We write an *action* that returns the current index (and increments it):

> freshM :: State (MySt a) Int
> freshM = undefined

Similarly, we want an action that updates the frequency of a given
element `k`.

> updFreqM :: Ord a => a -> State (MySt a) ()
> updFreqM = undefined

And with these two, we are done

> mlabelM :: Ord a => Tree a -> State (MySt a) (Tree (a, Int))
> mlabelM (Leaf x)     =  do y <- freshM
>                            updFreqM x
>                            return (Leaf (x,y))
> mlabelM (Branch t1 t2) = do t1' <- mlabelM t1
>                             t2' <- mlabelM t2
>                             return (Branch t1' t2')

Now, our *initial* state will be something like

> initM :: MySt a
> initM = M 0 Map.empty

and so we can label the tree

~~~~~{.haskell}
ghci> let tree2   = Branch tree tree
ghci> let (lt, s) = runState (mlabelM tree2) initM

ghci> lt
Branch (Branch (Branch (Leaf ('a',0)) (Leaf ('b',1))) (Leaf ('c',2)))
       (Branch (Branch (Leaf ('a',3)) (Leaf ('b',4))) (Leaf ('c',5)))

ghci> s
M {index = 6, freq = fromList [('a',2),('b',2),('c',2)]}
~~~~~


The ST Monad and the IO Monad
=============================

See [STMonad](STMonad.html) for a final discussion about the ST monad and
comparison with the IO monad.


Credit
------

The first part of the lecture is a revised version of the lecture notes by
[Graham Hutton][0], January 2011

[0]: http://www.cs.nott.ac.uk/~gmh/monads