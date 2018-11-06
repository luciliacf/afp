---
title: The Maybe and List Monads
date: October 23, 2017
---

> {-# LANGUAGE NoImplicitPrelude, KindSignatures, InstanceSigs #-}
> module Monads where
> import Prelude hiding ((>>))
> import Control.Monad (guard)


Warm-up exercise
----------------

Consider the definition of trees with values at their *leaves*.

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>    deriving (Eq, Show)

Define the following function that combines together the data stored
in the tree.

> -- | zip two trees together
> zipTree :: Tree a -> Tree b -> Tree (a,b)
> zipTree = undefined

        o                     o                      o
       / \                   / \                   /   \
     "a"  o        ===>     0   o        ===>  ("a",0)  o
         / \                   / \                     / \
       "b" "c"                1   2             ("b",1)  ("c", 2)

> testZip0 :: Bool
> testZip0 =
>   zipTree (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
>           (Branch (Leaf 0  ) (Branch (Leaf 1  ) (Leaf 2  )))
>   ==
>   (Branch (Leaf ("a",0)) (Branch (Leaf ("b",1)) (Leaf ("c",2))))


Keeping track of errors
-----------------------

What is tricky about this problem?  The trees must have the same shape in
order to be zipped together. There is no sensible result when the first
argument is `Leaf x` and the second is `Branch t1 t2`. This function is
inherently partial.

How could we define `zipTree` so that we can recover from failure? That\'s right,
we\'ll have it return a `Maybe`!

Let\'s rewrite it so that the partiality is explicit in the type.

> zipTree1 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> 
> zipTree1 (Leaf a)     (Leaf b)       = Just (Leaf (a,b))
> zipTree1 (Branch l r) (Branch l' r') =
>    case zipTree1 l l' of
>      Nothing -> Nothing
>      Just x  -> case zipTree1 r r' of
>                   Nothing -> Nothing
>                   Just y  -> Just (Branch x y)
> zipTree1 _            _              = Nothing

This function is going to be our inspiration for the following development.
We are going to factor out the repeated code in its definition. Therefore,
as we do this refactoring we will want to do regression tests.

> testZip :: (Tree String -> Tree Int -> Maybe (Tree (String, Int))) -> Bool
> testZip zt =
>   zt (Branch (Leaf "a") (Branch (Leaf "b") (Leaf "c")))
>      (Branch (Leaf 0  ) (Branch (Leaf 1  ) (Leaf 2  )))
>       ==
>       Just (Branch (Leaf ("a",0)) (Branch (Leaf ("b",1)) (Leaf ("c",2))))
>   &&
>   zt (Branch (Leaf "a") (Leaf "b")) (Leaf 0) == Nothing

We can test it with the following code:

       ghci> testZip zipTree1


       SPOILER SPACE BELOW. Don't look until you finish zipTree1.

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
       |
       |
       |
       |
       |
       |
       |





Abstracting programming patterns
================================

Here\'s what my solution to the `zipTree` problem looks like.
It\'s not terribly beautiful.


> zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree2 (Leaf a)     (Leaf b)       = Just (Leaf (a,b))
> zipTree2 (Branch l r) (Branch l' r') =
>    case zipTree2 l l' of
>      Nothing -> Nothing
>      Just x  -> case zipTree2 r r' of
>                   Nothing -> Nothing
>                   Just y  -> Just (Branch x y)
> zipTree2 _            _              = Nothing

All that nested pattern matching! This version looks so much worse than the
original (partial) version. It\'s hard to see the overall structure of
the code.

And, as you might guess, anytime we use a `Maybe` type this
sort of pattern is going to show up.

So let\'s try to do better!

Can you identify any patterns in the code?

  * How do we *return* a value? Is there a common pattern?

  * How do we *use* a value? Is there a common pattern? A helper
    function that we can define to factor out the pattern
    matching?


Looking closely for patterns
----------------------------

*  We return a value in two places in this function.  See the lines
   marked `(*)` below.

~~~~~~~~~{.haskell}
       zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
       zipTree2 (Leaf a)     (Leaf b)       = Just (Leaf (a,b))     (*)
       zipTree2 (Branch l r) (Branch l' r') =
         case zipTree2 l l' of
           Nothing -> Nothing
           Just x  -> case zipTree2 r r' of
                         Nothing -> Nothing
                         Just y  -> Just (Branch x y)               (*)
       zipTree2 _            _              = Nothing
~~~~~~~~~~

  In both cases we have a successful answer and we mark this success
  using the `Just` data constructor.

  As a reminder here is the type of `Just`

       Just :: a -> Maybe a

  Let\'s give this part of the program a good name, so that we can make it
  clear that we are returning a successful value.

> retrn :: a -> Maybe a
> retrn = Just

* We also *use* a pattern in two cases in the code. In two cases, we
  pattern match the result of a recursive call, and if it is successful,
  we use that successful result later on in the computation. See the
  parts marked `(*)` below.

~~~~~~~~~{.haskell}
       zipTree2 :: Tree a -> Tree b -> Maybe (Tree (a,b))
       zipTree2 (Leaf a)     (Leaf b)       = Just (Leaf (a,b))
       zipTree2 (Branch l r) (Branch l' r') =
         case zipTree2 l l' of   <------------------------ (*)
           Nothing -> Nothing
           Just x  -> case zipTree2 r r' of  <------------ (*)
                         Nothing -> Nothing
                         Just y  -> Just (Branch x y)
       zipTree2 _            _              = Nothing
~~~~~~~~~~~

  i.e\. the two parts of the code we are focussing are look
  like this

     case zipTree l l' of
       Nothing -> Nothing
       Just x -> ...
           do something with x

     case zipTree r r' of
       Nothing -> Nothing
       Just y -> ...
          do something with y

  We can see the general pattern if we abstract the scrutinee of the
  case analysis (let's call it `x`) and if we abstract the
  \"do something with \.\.\. \" (let call it `f`).  In other words, the general
  pattern is

     case x of
       Nothing -> Nothing
       Just y -> f y

  where

     x :: Maybe (Tree (a,b))

  and `f` is a function that says what we will do with this intermediate
  result.

     f :: Tree (a,b) -> Maybe (Tree (a,b))

  We\'ll give this pattern a name (\"bind\")

> bind x f = case x of
>    Nothing -> Nothing
>    Just y  -> f y

  and ask ghci what it\'s most general type is:

     ghci> :t bind
     bind :: Maybe t -> (t -> Maybe a) -> Maybe a

With these two new functions (`retrn` and `bind`) we can refactor the code.
Rewrite `zipTree` using them in place of `Just` and explicit case analysis.

> zipTree3 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree3 (Leaf a)     (Leaf b) = retrn (Leaf (a,b))
> zipTree3 (Branch l r) (Branch l' r') =
>     zipTree3 l l' `bind` (\x ->
>       zipTree3 r r' `bind` (\y -> return (Branch x y)))

> zipTree3 _ _ = Nothing

Do the names `retrn` and `bind` sound familiar to you? Do their types look
familiar?


Monads in Haskell
=================

These operations are the two components of the `Monad` type class.  The
general concept of a monad comes from a branch of mathematics called category
theory.  In Haskell, however, a monad is simply a parameterised type `m`,
together with two functions of the following types:

~~~~~{.haskell}
return :: a -> m a
(>>=)  :: m a -> (a -> m b) -> m b
~~~~~

The notion of a monad can now be captured as follows:

~~~~~{.haskell}
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
~~~~~

That is, a monad is a parameterised type `m` that supports `return` and `>>=`
(i.e\. bind) functions of the specified types.  The fact that `m` must be a
parameterised type, rather than just a type, is inferred from its use in the
types of return and `>>=`.

(To form a well-defined monad, the two functions are also required to
satisfy some simple properties; we will return to these later.)

The two operations we derived above are exactly what makes the parameterized
type `Maybe` a monadic type.

~~~~~{.haskell}
instance Monad Maybe where
   return      :: a -> Maybe a
   return x       =  Just x

   (>>=)       :: Maybe a -> (a -> Maybe b) -> Maybe b
   Nothing  >>= _ =  Nothing
   (Just x) >>= f =  f x
~~~~~

(*Aside*: to include types in instance declarations, you must enable the
InstanceSigs language extension.)

Note that the definitions of `return` and `>>=` in this instance
are exactly the same ones that we derived for `retrn` and `bind`!

So we can rewrite the example to use the monadic functions!

> zipTree4 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree4 (Leaf a)     (Leaf b) = return (Leaf (a,b))
> zipTree4 (Branch l r) (Branch l' r') =
>     zipTree3 l l' >>= (\x ->
>       zipTree3 r r' >>= (\y ->
>         return (Branch x y)))
> zipTree4 _ _ = Nothing

What is the benefit to writing the code this way?

- First, using `return` and `>>=` encapsulates a common pattern of plumbing
  when working with `Maybe`s. Note that `zipTree4` does not need to do any of
  the case analysis. That is handled automatically by `>>=`. We\'ve saved a
  little effort because it is common that we will need to combine several
  `Maybe` computations into one.

- The second benefit is that we get to use the `do` notation to make our code
  even prettier.


Monads and the do notation
==========================

So what does the Monad type class have to do with the `do` notation?
Well, remember that Haskell automatically transforms code of this form

~~~~~{.haskell}
do x1 <- m1
   x2 <- m2
   ...
   xn <- mn
   f x1 x2 ... xn
~~~~~

Into the following sequence of nested binds.

~~~~~{.haskell}
m1 >>= (\x1 ->
  m2 >>= (\x2 ->
     ...
      mn >>= (\xn ->
        f x1 x2 ... xn)...))
~~~~~

So much easier to read with the arrows flipped around, no?

With this translation, we can put the notation to use. Try rewriting the
`zipTree` function using the above `do` notation for the `>>=` operator.

> zipTree5 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree5 (Leaf a)     (Leaf b)       = return (Leaf (a,b))
> zipTree5 (Branch l r) (Branch l' r') = do
>       undefined
> zipTree5 _ _ = Nothing

Nice!

But wait, there\'s more.  Sometimes with do notation, we see a line in
the middle that *doesn\'t* bind a variable.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
        main = do
          x <- doSomething
          doSomethingElse     -- what is going on here?
          y <- andSoOn
          f x y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In fact, we saw this sort of thing when using the `IO` monad at the beginning
of the semester. (Yep, the `IO` type constructor is an instance of the `Monad`
type class.  Under the covers, it is all binds and returns.)

> main :: IO ()
> main = do
>    putStrLn "Hello. What is your name?"
>    inpStr <- getLine
>    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"


Sometimes we don\'t need to use the result of a computation \-\- in
particular, if the computation has type `IO ()` then the result is
trivial. However, we still want to use bind for it\'s sequencing
capabilities.

That brings us to a derived monad operator, called \"sequence\":

> (>>)  :: Monad m => m a -> m b -> m b
> m1 >> m2 = m1 >>= \_ -> m2

So the do notation above transforms to:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
       doSomething >>= ( \x ->
         doSomethingElse >>         -- it was just a sequence
           (andSoOn >>= ( \y ->
             f x y)))
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Applicative Functors
==============================

There is yet another way to implement `zipTree`.

Let\'s take a look at the documentation for the [Monad
Class](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Monad.html).

A fairly recent change to Haskell\'s core library design is that the class
`Applicative` is a superclass of `Monad`. Any type constructor, like `Maybe`
that is an instance of `Monad` must also be an instance of `Applicative`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
      class Applicative m => Monad m where
          ...
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The name `Applicative` is short for \"applicative functor\". What are these
 things?  They are functors with extra features.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
      class Functor f => Applicative f where
          pure   :: a -> f a
          (<*>)  :: f (a -> b) -> f a -> f b
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In other words, an `Applicative` is a functor with application. It provides
operations to embed pure expressions (`pure`), and sequence computations while
combining their results (`<*>`). (This operation is called "zap".)

Like `Functor`, this class captures useful functions for working with data
structures. Let\'s look at the `Applicative` instance for `Maybe`.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
   instance Applicative Maybe where
      pure :: a -> Maybe a
      pure = Just

      (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
      Just f  <*> Just x = Just (f x)
      _       <*> _      = Nothing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For `Maybe`, the `<*>` operation applies a function to an argument, provided
that they are both defined.

Here\'s how we could use these operations in `zipTree` :

> zipTree6 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree6 (Leaf a)     (Leaf b)       = pure (Leaf (a,b))
> zipTree6 (Branch l r) (Branch l' r') =
>      pure Branch <*> zipTree6 l l' <*> zipTree6 r r'
> zipTree6 _ _ = Nothing

The `<*>` operator lets us \"lift\" the `Branch` data constructor to the
partial results of the left and right recursive calls.

Furthermore, it is a required law of applicative functors that the following
relationship must hold (recall that `<$>` is an infix operator for `fmap`):

         pure f <*> x  ==  f <$> x

That gives us one more tweak:

> zipTree7 :: Tree a -> Tree b -> Maybe (Tree (a,b))
> zipTree7 (Leaf a)     (Leaf b)       = pure (Leaf (a,b))
> zipTree7 (Branch l r) (Branch l' r') =
>      Branch <$> zipTree7 l l' <*> zipTree7 r r'
> zipTree7 _ _ = Nothing

Compare this version to our initial (unsafe) one. Making this code safe comes
at very little cost.


Functors, Applicatives and Monads
---------------------------------

Note that the superclass constraint of the Monad type class means that *any*
monads that you define must also have `Applicative` and `Functor` instances.
However, this is not much of a burden: there is a straightforward definition
of `Functor` and `Applicative` functions in terms of `return` and `(>>=)`.

See if you can define functions with the correct types below *only* using
`return` and `>>=`. (We will justify these definitions later, when we talk
about the general properties that all functors, applicatives and monads must
satisfy. However, if you can define something that type checks, it is probably
right. )

> monad_fmap :: (Monad m) => (a -> b) -> m a -> m b
> monad_fmap f m = m >>= return . f

> monad_pure :: (Monad m) => a -> m a
> monad_pure = return

> monad_zap  :: (Monad m) => m (a -> b) -> m a -> m b
> monad_zap m1 m2 = m1 >>= \f -> m2 >>= \x -> return (f x)

Note that `monad_fmap` is called `liftM` and `monad_zap` is called `ap` in the
Control.Monad library.

With these functions it is trivial to construct instances of Functor and
Applicative given an instance of Monad.  Of course, you may not want to always
do that: sometimes a specialized definition will be more efficient.


The List Monad
==============


The `Maybe` monad provides a simple model of computations that can
fail, in the sense that a value of type `Maybe a` is either `Nothing`,
which we can think of as representing failure, or has the form
`Just x` for some `x` of type `a`, which we can think of as success.

The list monad generalises this notion by permitting _multiple_
results in the case of success.  More precisely, a value of `[a]` is
either the empty list `[]`, which we can think of as failure, or a
non-empty list `[x1,x2,...,xn]`, which we can think of as a list of
successes.


A challenge
-----------

Write the function that takes each possible value `x` from the list
`xs`, and each possible value `y` from the list `ys` and returns a
list of the `(x,y)` pairs.

Hint: you should use the functions `concat :: [[a]] -> [a]` and
`map :: (a -> b) -> [a] -> [b]` in your solution.  Your solution itself
should *not* be recursive.

> pairs0 :: [Int] -> [Int] -> [(Int,Int)]
> 
> pairs0 xs ys = concat (map (\x ->
>                  concat (map (\y ->
>                    [(x,y)]) ys)) xs)


> testPairs ps = ps [1,2,3,4] [5,6,7,8] ==
>   [(1,5),(1,6),(1,7),(1,8),(2,5),(2,6),(2,7),(2,8),
>    (3,5),(3,6),(3,7),(3,8),(4,5),(4,6),(4,7),(4,8)]


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

> pairs1 :: [Int] -> [Int] -> [(Int,Int)]
> pairs1 xs ys = concat (map (\x ->
>                  concat (map (\y ->
>                    [(x,y)]) ys))
>                   xs)

Can we divide this up? What are the patterns here?

We have

     concat (map (\x ->  do something with x) xs)

     concat (map (\y ->  do something with y) ys)

Generalize:

     concatMap f xs

where

     concatMap = concat . map

What is the type of `concatMap`

     concatMap :: (a -> [b]) -> [a] -> [b]

AHA!! That type looks familiar.  We can define

     (>>=) :: [a] -> (a -> [b]) -> [b]
     xs >>= f = concatMap f xs

What could `return` be?

     return :: a -> [a]
     return x = [x]

This is how we lists can be monadic.

~~~~~{.haskell}
    instance Monad [] where
       return :: a -> [a]
       return x  =  [x]

       (>>=)  :: [a] -> (a -> [b]) -> [b]
       xs >>= f  =  concatMap f xs
~~~~~

(*Aside*: in this context, `[]` denotes the list type `[a]` without
its parameter.)

So `return` simply converts a value into a (singleton) successful
result, while `>>=` provides a means of sequencing computations that
may produce multiple results: `xs >>= f` applies the function `f` to
each of the values in `xs` to give a list of lists of results, which
is then concatenated to give a single list of results.

Rewrite `pairs` using `>>=` and return

> pairs2 :: [Int] -> [Int] -> [(Int,Int)]
> pairs2 xs ys = xs >>= \x -> ys >>= \y -> return (x,y)


Rewrite again using do notation

> pairs3 :: [Int] -> [Int] -> [(Int,Int)]
> pairs3 xs ys = do
>    x <- xs
>    y <- ys
>    return (x,y)

Make sure that it still works.

> testPairs2 = testPairs pairs2
> testPairs3 = testPairs pairs3








List comprehensions
-------------------

It is interesting to note the similarity to how this function would be
defined using list comprehension notation:

> pairs4 xs ys = [ (x,y) | x <- xs, y <- ys ]

The reason is that the list comprehension syntax in Haskell is defined in
terms of `>>=` and `return`.  Each `x <- xs` corresponds to a use of `>>=`,
just as in do-notation. The final list is constructed with `return`.

List comprehensions can also include *guards*, i.e\. boolean expressions that
filter out some of the results

> pairs5 xs ys = [ (x,y) | x <- xs, y <- ys, x /= y ]

Compare the versions with and without the guard.

    ghci> pairs4 [1,2,3] [1,2,3]
    ghci> pairs5 [1,2,3] [1,2,3]

We can also rewrite the guard version with do notation using the following
guard function:

~~~~~{.haskell}
     guard :: Bool -> [()]
     guard True      =  [()]
     guard False     =  []
~~~~~

> pairs5' xs ys = do
>     x <- xs
>     y <- ys
>     guard (x /= y)    --- remember that no `<-` means `>>`, i.e. `>>=` ignoring the argument
>     return (x,y)


In fact, there is a formal connection between the `do` notation and
the comprehension notation.  Both are simply different shorthands for
repeated use of the `>>=` operator for lists.  Indeed, the language
*Gofer*, one of the precursors to Haskell, permitted the comprehension
notation to be used with *any* monad.  For simplicity, Haskell only
allows comprehension to be used with lists.


Let\'s play around with list comprehensions in case you have not seen them in
other contexts.

What are some other examples that can be written using list comprehension?

* We can find out the smallest number colors that can color a few southern states so that
  neighboring states are not the same color.

Here are some colors

> data Color = Red | Green | Blue | Yellow | Orange | Violet deriving (Show, Enum, Eq)

And, given a list of colors, this function determines all of the ways the five states
can be colored with them:

> stateColors :: [Color] -> [(Color, Color, Color, Color, Color)]
> stateColors colors =
>   [(tennessee, mississippi, alabama, georgia, florida) |
>    tennessee   <- colors,
>    mississippi <- colors,
>    alabama     <- colors,
>    georgia     <- colors,
>    florida     <- colors,
>    tennessee   /= mississippi,   -- ensure neighboring states have different colors
>    tennessee   /= alabama,
>    tennessee   /= georgia,
>    mississippi /= alabama,
>    alabama     /= georgia,
>    florida     /= alabama,
>    florida     /= georgia]

And this code finds the smallest number of colors that can do so:

> colorsNeeded = head (filter (not . null . stateColors) cs) where
>     cs = zipWith take [1..] (replicate 6 [Red ..])

Other examples
--------------


* Rewrite the `map` function using a list comprehension.

> map' :: (a -> b) -> [a] -> [b]
> map' f xs = [ f x | x <- xs ]


* Create a list of all pairs where the first component is strictly less than the second.

> firstLess  :: Ord a => [a] -> [a] -> [(a,a)]
> firstLess xs ys = [ (x,y) | x <- xs, y <- ys, x < y]

> testFirstLess fl = fl [1,2,3,4] [1,2,3,4] == [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]



Now rewrite using do notation (don\'t forget `guard` above)

> map1 :: (a -> b) -> [a] -> [b]
> map1 f xs = do
>     x <- xs
>     return (f x)

> firstLess1 :: Ord a => [a] -> [a] -> [(a,a)]
> firstLess1 xs ys = do
>     x <- xs
>     y <- ys
>     guard (x < y)
>     return (x,y)


* Rewrite `filter`, using a guarded list comprehension.

> filter' :: (a -> Bool) -> [a] -> [a]
> filter' f xs = [ x | x <- xs, f x]



The List Applicative
====================

Like Maybe, there is also an instance of the `Applicative` type class for
lists.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
   instance Applicative [] where
      pure :: a -> [a]
      pure x = [x]

      (<*>) :: [a -> b] -> [a] -> [b]
      fs <*> xs = [f x | f <- fs, x <- xs]

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Can we rewrite the pair example using Applicative instance instead? Take a look
at the definition above to see what could makes sense.

> pairs6 xs ys = pure (,) <*> xs <*> ys