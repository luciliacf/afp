---
title: Monads 
date: September 25, 2017
---

[monadslhs]: Monads.lhs

*Note:* You may download the [lhs version][monadslhs]
of this module.

Now, the most famous of all Haskell type classes: The warm fuzzy
thing called \"Monad\", that captures another pattern of effectul programming.

We saw an example of the `IO` monad with code like this:

> main :: IO ()
> main = do
>    putStrLn "This is the Advanced FP course. What is your name?"
>    inpStr <- getLine
>    putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
>    return ()

This code works because `IO` is an instance of the `Monad` type
class. We\'ll see more instances of this class in the next few
lectures. Don\'t try to understand it all at once!  We\'ll start with
just looking at what\'s going on at a syntactic level.

~~~~~{.haskell}
class Applicative m => Monad m  where

   -- | Inject a value into the monadic type.
   return      :: a -> m a

   -- | Sequentially compose two actions, passing any value produced
   -- by the first as an argument to the second. Also called "bind"
   (>>=)       :: m a -> (a -> m b) -> m b

   -- | Sequentially compose two actions, discarding any value produced
   -- by the first, like sequencing operators (such as the semicolon)
   -- in imperative languages.
   (>>)        :: m a -> m b -> m b
   m >> k      = m >>= \_ -> k         -- default definition

   -- | Fail with a message.  This operation is not part of the
   -- mathematical definition of a monad, but is invoked on pattern-match
   -- failure in a @do@ expression.
   fail        :: String -> m a
   fail s      = error s              -- default definition
~~~~~~~~~~~

You can see the use of `return` in the last line of the `main` function above.
In fact, it must be the last line in any computation because it doesn\'t compose
multiple actions together.

> nop :: IO ()
> nop = do
>         return ()

We\'ve also been using `(>>=)`, but only behind the scenes. You\'ve
missed it because of another feature of Haskell \-\- the \"do\" syntax for
composing sequences of actions.

For example, code like this

~~~~~{.haskell}
main :: IO ()
main = do
  x <- doSomething
  doSomethingElse
  y <- andSoOn
  return ()
~~~~~~~~~~~~~

is really shorthand for this:

~~~~~{.haskell}
doSomething >>= ( \x ->
  doSomethingElse >>
    (andSoOn >>= ( \y ->
      return () )))
~~~~~~~~~~~~

So everytime that you see `do` there is some monad involved, though
not necessarily `IO`!  Let\'s start by a very simple instance instance of the `Monad`
typeclass that will help to understand its concepts.

The Identity Monad
-------------------

So, the `Monad` type class is a common API for talking to a bunch of different types.
So the question is this: what\'s so special about this API?

APIs often capture design patterns, and *the design pattern here is a one-way wrapper*.
We can wrap things but we can\'t unwrap them. We still want to be able to whatever we like
to the wrapped data, but anything that depends on anything wrapped should itself be wrapped.

Here is the simpler wrapper type, that we will call `Id` (for identity wrapper):

> data Id a = Id a deriving Show

Note how it doesn\'t add anything except some wrapping. And the first thing we need is to be
able to wrap anything we like. We could simply do that by using type constructo `Id`, but we
are heading towards a common API that will work with other types too, so it obviously can\'t be
called `Id`. Thus, we could define a function:

>return' :: a -> Id a
>return' x = Id x

But we also need one more thing - a way to manipulate wrapped data leaving it wrapped.
There\'s an obvious idea. Given any function `a -> b` we write a function that converts
it to a function `Id a -> Id b`. This is guaranteed to keep things under wraps.
So here goes:

>     instance Functor Id where 
>        --- fmapId :: (a -> b) -> (Id a -> Id b)
>        fmapId f (W x) = W (f x)

Now we could, for example, wrap a number and increment it (without unwrapping):

> a' = Id 1
> b' = fmap (+1) a 

But here is something we can\'t do. Let's define a function `f` like this:

> f x = Id (x+1)

It increments and returns a wrapped result. Now suppose we want to apply the underlying operation
here twice, ie. we\'d like to increment a number twice and return a wrapped result. We can\'t simply
apply `f` twice because (1) it\'ll doubly wrap our result and (2) it\'ll attempt to add `1` to a wrapped
result, something that doesn\'t make sense. `fmap` doesn\'t do what we want either. Try it in an interactive
Haskell session. It seems we need a way to apply `f`, unwrap the result and apply `f` again.
But we\'ve already said that we don\'t want to allow people to unwrap these things.
We we need to provide a higher order function that does the unwrapping and application for us.
As long as this function always gives us back something that is wrapped, our end users will never
be able to unwrap anything. Here\'s an idea for such a function:

>bind' :: (a -> Id b) -> (Id a -> Id b)
>bind' f (W x) = f x

Notice how it\'s very similar to `fmap` but is even simpler. So now we can try doubly, or triply, incrementing:

> c' = bind' f (f 1)
> d' = bind' f (bind' f (f 1))

And that\'s it. Using `return'` and `bind'` we have achieved our goal of wrapping objects and freely manipulating
wrapped objects while keeping them wrapped. What\'s more, we can chain functions that wrap without getting bogged
down in multiple layers of wrapping. And that, really, sums up what a Haskell monad is all about.

In other words, we need simply define an `Id` instance of typelcass `Monad`:

> instace Monad Id where
>    return x = Id x
>    f >>= (Id x) = f x  

Notice how `bind` (thet is, `(>>=)`) is more general than `fmap`. In fact, `fmap f = bind (return . f)`.
Thus any type constructor that is an instace of the `Monad` typeclass can be made an instace of the
`Functor` typeclass.

The above definitions can now be rewitten (using the `do` notation) as:

> a,b,c,d :: Id a
> a = return 1
> b = a+1 >>= (\y -> return y) 
> c = f 1 >>= (\y -> return (f y))
> d = f 1 >>= (\y -> f y >>= \z -> return (f z))

Or, using the `do` notation:

> bd, cd, dd :: Id a
> bd = do { return (a+1) }
> cd = do { y <- f 1; z <- f y; return (f z) } 

Monad Laws
------------

In a similar way to functors and applicatives, the two monadic primites are required to satisfy some
basic laws:

     return x >>= f   = f x
     mx >>= return    = mx
     (mx >>= f) >>= g = mx >>= (\x -> (f x >>= g))

The first equation says that if we `return` a value and then feed it into monadic function, this should
give the same result as applying the function to the value. Dually, the second equation states that if
we feed the result of a monadic computation into the function `return`, this should give the same result
as simply performing the computation. Together, these two equetions state, modulo the fact that the second
argumento to `>>=` involves a binding operation, that `return` is the identity for the `>>=` operator.

The third equation expresses (again modulo binding) that `>>=` is associative. 

**Notes**:

  * The `Monad` type class is exported by the `Prelude`, along with a few standard instances.
    However, many utility functions are found in `Control.Monad`.

  * You can take a look at several instance definitions for the `Monad` typeclass [here][monadInstances]

[monadInstances]: http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad-Instances.html#t:Monad


Reading
-------

  * Miran Lipovaca, *Learn You a Haskell for Great Good!*, chapter 8 (IO Monad).
  
  * Graham Hutton, *Programming in Haskell* , 2nd Ed., chapter 12 (section 12.3).

  * Dan Piponi, [The Trivial Monad][piponi]


[piponi]: http://blog.sigfpe.com/2007/04/trivial-monad.html