---
title: Functors, Applicatives and Monads
date: September 25, 2017
---

[category]: https://bartoszmilewski.com/2011/01/09/monads-for-the-curious-programmer-part-1/
[currying]: https://wiki.haskell.org/Currying

This lecture introduces `Functor`, `Applicative` and `Monad` type classes, which capture
generic notions of mapping, function application and *effectful programming*,
abstracting out common programming patterns as a definition. These ideas are  
founded on concepts from the *Category theory* (a nice introduction to Category theory
for programmers can be found [here][category]).


> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, InstanceSigs #-}
> {-# OPTIONS -fno-warn-type-defaults #-}
> module MoreClasses where

> import Test.HUnit
> import Data.Char
> import Text.Read
> import Prelude hiding (lookup)


Functor
=======

Recall the `map` function on lists

~~~~~{.haskell}
map :: (a -> b) -> [a] -> [b]
~~~~~~~~~~~~

that takes a function and applies it to every element of a list,
creating a new list with the results.  We also saw that the same
pattern can be used for `Tree`s:

~~~~~{.haskell}
treeMap :: (a -> b) -> Tree a -> Tree b
~~~~~~~~~~~~

If you think a little, you\'ll realize that `map` makes sense for
pretty much any data structure that holds a single type of values.  It
would be nice if we could factor this pattern out into a class, to
keep track of the types that support `map`.

Behold, `Functor`:

~~~~~{.haskell}
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
  ...
~~~~~~~~~~

That is, for a parameterized type `f` to be an instance of the class `Functor`,
it must support a function `fmap` on the specified type. The intuition is that
`fmap` takes a function of type `a -> b` and a structure of type `f a ` whose
elements have type `a`, and applies the function to each such element to give a
structure of type `f b`. 

`Functor` is a little different than the other classes we\'ve seen so
far.  It\'s a \"constructor\" class, because the types it works on are
constructors like `Tree`, `Maybe` and `[]` \-\- ones that take another
type as their argument.  Notice how the `f` in the class declaration
is applied to other types.

The standard library defines:

~~~~~{.haskell}
instance Functor [] where
  -- fmap :: (a -> b) -> [a] -> [b]
  fmap = map
~~~~~~~~~~

We can define a `Functor` instance for our own trees:

> instance Functor Tree where
>   fmap = treeMap where
>     treeMap f Empty = Empty
>     treeMap f (Branch x l r) = Branch (f x) (treeMap f l) (treeMap f r)

The standard library also defines `Functor` instances for a number of
other types.  For example, `Maybe` is a `Functor`:

~~~~~{.haskell}
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing
~~~~~~~~~~

`Functor` is very useful, and you\'ll see many more examples of it in
the weeks to come.

See if you can define a `Functor` instance for this type:

> data Two a = MkTwo a a deriving (Eq, Show, Read, Ord)

> instance Functor Two where
>     fmap = undefined     -- provide a definition here

Many functors `f` that are used in Haskell are similar to the examples above, in the
sense that `f a` is a structure that contains elements of type `a`, which is sometimes
called a *container type*, and `fmap` applies a function to each such element. However,
not all instances fit this pattern. For example, the `IO` type is not a container in
the normal sense of the term because its values represent input/output actions whose
internal structure we do not have acces to, but it can readly be made into a functor:

~~~~~{.haskell}
instance Functor IO where
  -- fmap :: (a -> b) -> IO a -> IO b
  fmap g mx = do { x <- mx; return (g x) } 
~~~~~~~~

In this case, `fmap` applies a function to the result value of tyhe argument action,
and hence provides a means of processing such values. for example:

~~~~~{.haskell}
ghci> fmap show (return True)
"True"
~~~~~~~~~

Let\'s try now something a bit more mind-twisting:

~~~~~{.haskell}
instance Functor ((->) e) where
~~~~~~~~

What!? Well, let\’s follow the types: if `f = (->) e` then we want

~~~~~{.haskell}
fmap :: (a -> b) -> (->) e a -> (->) e b
~~~~~~~~~~

or, with `(->)` written infix:

~~~~~{.haskell}
fmap :: (a -> b) -> (e -> a) -> (e -> b)
~~~~~~~~~~

Hmm, this type signature seems familiar

~~~~~{.haskell}
instance Functor ((->) e) where
  fmap = (.)
~~~~~~~~~

What does this mean? Well, one way to think of a value of type `(e -> a)` is as a \"e-indexed container\"  with
one value of `a` for each value of `e`. To map a function over every value in such a container corresponds exactly
to function composition: to pick an element out of the transformed container, we first we apply the `(e -> a)`
function to pick out an `a` from the original container, and then apply the `(a -> b)` function to transform the
element we picked.


**Remarks:**

  * The function `fmap` can be used to process the elements of any structure that is
    functional, which avoids that we have to use different names for each such function
    operating on a different structure.

  * We can define generic functions that can be used with any functor. For example, our
    earlier function `inc`, that increments each integer in a list, can be generalised to
    any functional type by simply using `fmap` rather than `map`:

> inc :: Functor f => f Int -> f Int
> inc = fmap (+)

For example:

~~~~~{.haskell}
ghci> inc (Just 2)
Just 3

ghci> inc (Branch 3 Empty (Branch 1 Empty Empty))
(Branch 4 Empty (Branch 2 Empty Empty))
~~~~~~~~~

  * Haskell also provides an infix version of `fmap`, defined by `g <$> x = fmap g x`.


Functor Laws
------------

Functors are required to satisfy two equational laws:

     fmap . id    = id                   -- identity
     fmap (g . h) = fmap g . fmap h      -- composition

The first equation states that `fmap` preserves the identity function, in the sense that applying
`fmap` to this function returns the same function itself. Note, however, that the two occurrences
`id` in this equation have different types: on the left-hand side, `id` has type `a -> a`and hence
`fmap id` has type `fa -> f a`, which means that `id` on the right-hand side must also have type
`f a -> f a` in order for the equation to be well-typed.

The second equation above states that `fmap` also preserves function composition, in the sense that
applying `fmap` to the composition of two functions gives the same result as applying `fmap` to the
two functions separately and then composing. In order for the composition to be well-typed, the
component functions `g` and `h` must have types `b -> c` and `a -> b`, respectively.

The functor laws ensure that `fmap` does indeed perform a mapping operation. In fact, it can be proved
that, for any parameterised type in Haskell, there is at most one function `fmap` that satisfies the
required laws. That is, if it is possible to make a given parameterised type into a functor, there is only
one way to achieve this. Hence, the instances we"\'ve defined for lists, `Maybe`, `Tree` and `IO` were all
uniquely determined.


Applicative
=============

Functors abstract the idea of mapping a function over each element of a structure.
Suppose now that we want to generalise this idea to allow functions with any number of arguments
to be mapped, rather than being restricted to functions of with a single argument.
More precisely, suppose we want to define a hierachy of `fmap` functions with
the following types:

     fmap0 :: a -> f a
     fmap1 :: (a -> b) -> f a -> f b
     fmap2 :: (a -> b -> c) -> f a -> f b -> f c
     fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
     ...

Note that `fmap1` is just another name for `fmap`,  and `fmap0` is the degenerate case when the
function being mapped has no arguments. However, we would not want to declare a special version of the
functor class for each of this cases.
Besides, it is not clear how many syuch classes we should declare, as there are infinitely many.

If we view `fmap`of type `(a -> b) -> f a -> f b` as a generalisation of the buit-in function application
operator of type `(a -> b) -> a -> b`, we might expect that some form of [*currying*][currying] can be
used to achieve the desired behavour. In fct, using the idea of currying, it turns out that that a version
of `fmaap` for functions of any desired number of
arguments can be constrtucted in terms of the two basic functions below, defined in typeclass `Applicative`: 

~~~~~{.haskell}
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
~~~~~~~~~~~~~

That is, `pure` converts a value of type `a` into a structure of type `f a`, while `<*>` is a generalised form of
function application for which the argument function, the argument value and the result value are all contained
in `f` structures. As with normal function application, the `<*>` operator is written between its two arguments and
associates to the left.

A typical use of `pure` and `<*>`has the following form:

~~~~~{.haskell}
pure g <*> x1 <*> x2 <*> ... <*> xn
~~~~~~~~~

Such expressions are sad to be in *applicative style*, because of the similarity to normal function application
notation `g x1 x2 ... xn`. In both cases, g is a curried function that takes `n` arguments of types `a1, a2, ... an`,
and produces a result of type `b`. However, in applicative style, each argument `xi` has type `f  ai`, rather than
just `ai`, and the overall result has type `f b`, rather than just `b`.

Using this idea we can now define a hierachy of mapping functions:

     fmap0 :: a -> f a
     fmap0 = pure

     fmap1 :: (a -> b) -> f a -> f b
     fmap1 g x = pure g <*> x

     fmap2 :: (a -> b -> c) -> f a -> f b -> f c
     fmap2 g x y = pure g <*> x <*> y

     fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
     fmap3 g x y z = pure g <*> x <*> y <*> z

     ...

Let\'s see some example instances of the `Applicative` typeclass.

Using the fact that `Maybe` is a functor, and hence supports `fmap`, we can define:

~~~~~{.haskell}
instance Applicative Maybe where
  -- pure :: a -> Maybe a
  pure = Just
  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing <*> _  = Nothing
  Just g <*> mx  = fmap g mx
~~~~~~~~~~~

For example:

~~~~~{.haskell}
ghci> pure (+) <*> Just 2 <*> Just 3
Just 5

ghci> pure (+) <*> Just 2 <*> Nothing
Nothing
~~~~~~~~~~

How would we declare an instance of `Applicative` for lists? The following instance declaration
for lists is included in the Haskell standard `Prelude`:

~~~~~{.haskell}
instance Applicative [] where
  -- pure :: a -> [a]
  pure x = [x] 
  -- (<*>) :: [(a -> b)] -> [a] -> [b]
  gs <*> xs = [ g x | g <- gs, x <- xs ]
~~~~~~~~~~~

That is, `pure` transforms a value into a singleton list, while `<*>` takes a list of functions and
a list o arguments and applies each function to each value in turn, returning all the results in a list.
For example:

~~~~~{.haskell}
ghci> pure (+) <*> [1] <*> [2]
[3]

ghci> pure (*) <*> [1,2] <*> [3,4]
[3, 4, 6, 8]
~~~~~~~~~~~

Now, let us consider an instance of `Applicative` for type constructor `IO`:

~~~~~{.haskell}
instance Applicative IO where
  -- pure :: a -> IO a
  pure x = return
  -- (<*>) :: IO (a -> b) -> IO a -> IO b
  mg <*> mx = do { g <- mg; x <- mx; return (g x) ]
~~~~~~~~~~~~

In this case, `pure` is defined as the `return` functionfor the `IO` type and `<*>` applies an
impure function to an impure argument to give an impure result. For example, a function that reads
a given number of characters from the keyboard can be defined in applicative style as follows:

> getChars :: Int -> IO String
> getChars 0 = return []
> getChars n = pure (:) <*> getChar <*> getChars (n-1)

Note that the applicative style for `IO` supports a form of *interactive*  programming in which we
can apply pure functions to impure arguments without the need to manage the sequencing of actions or
the extraction of result values, as this is taken care automatically by the applicative machinery.

Let\'s do one final example instance, for `(->) e`. This is known as the reader or environment applicative,
since it allows “reading” from the \“environment” `e`. Implementing the instance is not too hard, we just
have to use our nose and follow the types:

~~~~~{.haskell}
instance Functor ((->) e) where
  fmap = (.)

instance Applicative ((->) e) where
  pure = const
  f <*> x = \e -> (f e) (x e)
~~~~~~~~~~

**Note:** Remember that `const x` is a unary function which evaluates to `x` for all inputs.


Efectful programming
---------------------

Note that the applicative operator `<*>` allows us to write programs in a familiar applicative style
in which functions are applied to arguments, with the difference that the arguments are no longer plain
values, but may also have *effects* \-\- the possibility of failure (`Maybe`), having many ways to succeed (lists),
performing input/output actions (`IO`) etc. Thus, applicative functors can also be viewed as abstracting
the idea of applying functions to effectful arguments, with the precise form of effects that are permited
depending on the nature of the underlying functor.

An important benefit is that we can define generic functions that can be used with any applicative functor.
for example, we could define the following function: 

> sequenceL :: Applicative f => [f a] -> f [a]
> sequenceL []     = pure []
> sequenceL (x:xs) = pure (:) <*> x <*> sequenceL xs

This function transforms a list of applicative actions into a single such action that returns a list of
result values, and capture a common pattern of applicative programming. For example, the function `getChars`
can be more simply defined as

> getChars :: Int -> IO String
> getChars n = sequenceL (replicate n getChar) 


Applicative laws
-----------------

Applicative functors are required to satisfy the following laws:

     pure id <*> x   = x                                -- identity
     pure (g x)      = pure g <*> pure x                -- homomorphism
     x <*> pure y    = pure (\g -> g y) <*> x           -- interchange
     x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z     -- composition

The first equation states that pure preserves identity, in the sense that
applying pure to this function gives an applicative version of identity.

The second equation states that pure also preserves function application, in the
sense that it distributes over normal function application to give applicative application.

The third equation states that when an effectful function is applied to a pure argument,
the order in which we evaluate the two components doesn\'t matter.

Finally, the fourth equation states that, modulo the types that are involved, the operator
`<*>` is associative. 

These laws ensure that every well-typed expression that is built using function `pure` and
operator `<*>` can be rewritten in the form (the fouth law associates applications to the left,
the third law moves occurrences of `pure` to the left and the other two laws allow zero or
more occurrences of `pure` to be combined into one):

~~~~~{.haskell}
pure g <*> x1 <*> x2 <*> ... <*> xn
~~~~~~~~~~

Haskell also provides an infix version of `fmap` defined by `g <$> x = fmap g x`.
In combination with the above law, this gives an alternative formulation for the
applicative style (note that, if `g :: a1 -> a2 -> ... -> an -> b`, then `g <$> x1` has
type `f (a2 -> ... -> an -> b)`):

~~~~~{.haskell}
g <$> x1 <*> x2 <*> ... <*> xn
~~~~~~~~~~~


Utility functions
-----------------

Module `Control.Applicative` provides several utility functions that work generically
with any `Applicative` instance.
Some of these are listed below:


 * `liftA :: Applicative f => (a -> b) -> f a -> f b`. This should be familiar; of course, it is the same as `fmap`
    (and hence also the same as `(<$>)`), but with a more restrictive type. This probably exists to provide a parallel
    to `liftA2` and `liftA3`, but there is no reason you should ever need to use it.

  * `liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c` lifts a 2-argument function to operate in the
     context of some `Applicative`. When `liftA2` is fully applied, as in `liftA2 f arg1 arg2`, it is typically
     better style to instead use `f <$> arg1 <*> arg2`. However, `liftA2` can be useful in situations where it
     is partially applied. For example, one could define a `Num instance` for `Maybe Integer`
     by defining `(+) = liftA2 (+)`
     and so on. There is also a `liftA3` but no liftAn for larger n.
       
  * `when :: Applicative f => Bool -> f () -> f ()` conditionally executes a computation, evaluating to its second
     argument if the test is `True`, and to `pure ()` if the test is `False`.
     
  * `unless :: Applicative f => Bool -> f () -> f ()` is like `when`, but with the test negated.


Reading
-------

  * Miran Lipovaca, *Learn You a Haskell for Great Good!*, chapter 10 and 11.
  
  * Graham Hutton, *Programming in Haskell* , 2nd Ed., chapter 12.

Further Reading
---------------
  * Michael Snoyman, Functor, [Applicative and Monad][Snoyman]

  * Brent Yorgey, [Monoids: Theme and Variations][Yorgey].

  * McBride and Paterson, [Applicative programming with effects][McBride].


[Snoyman]: https://www.schoolofhaskell.com/school/advanced-haskell/functors-applicative-functors-and-monads#decoupling-code 
[Yorgey]: http://ozark.hendrix.edu/~yorgey/pub/monoid-pearl.pdf
[McBride]: http://www.staff.city.ac.uk/~ross/papers/Applicative.html