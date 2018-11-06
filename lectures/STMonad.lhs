---
title: The ST and IO Monads
date: Oct 30, 2017
---

[STMonadlhs]: STMonad.lhs

Note: The [lhs version][STmonadlhs] of this file is available.

> module STMonad where

> import Control.Monad (forM_)
> import qualified Data.Vector.Mutable as V

In this example, we will discuss the connection between Haskell\'s
implementation of the `State` monad and the `IO` monad. GHC\'s version plays a
trick to allow a much more efficient implementation of the `State` monad than
we could do using pure Haskell.

> import Control.Monad.ST

Recall that interactive programs in Haskell are written using the
type `IO a` of \"actions\" that return a result of type `a`, but may
also perform some input/output.  A number of primitives are
provided for building values of this type, including:

~~~~~{.haskell}
return  :: a -> IO a
(>>=)   :: IO a -> (a -> IO b) -> IO b
~~~~~

The presence of `return` and `>>=` means that we can treat `IO` as a
monad, and hence (as we\'ve seen) that the `do` notation can be used to
write imperative programs.

> getFullLine :: IO String
> getFullLine = getChar >>= (\c ->
>           if c == '\n' then return []
>           else getFullLine >>= (\cs -> return (c:cs)))



One way to understand the `IO` monad is as a special case of the State monad,
in which the internal state is a suitable representation of the \"state of the
world\":

~~~~~{.haskell}
   type RealWorld = ...

   type IO a  = RealWorld -> (a,RealWorld)
~~~~~

That is, an IO action can be viewed as a function that takes the current state
of the world as its argument, and produces a value and a modified world as its
result. In other words, we have this correspondence:

In reality, Haskell systems such as GHC implement actions in a more
efficient manner, but for the purposes of understanding the behavior of
actions, the above interpretation can be useful.

Mutable Arrays in Haskell
-------------------------

The two most important actions associated with the State Monad are accessing
and modifying the `Store`.

~~~~~~{.haskell}
   get :: State Store Store
   put :: a -> Store -> State Store ()
~~~~~~

What does this mean for the interpretation of the IO monad as the State monad,
using the `RealWorld` as the state?

~~~~~{.haskell}
   type IO a = State RealWorld a
~~~~~

In this case, `get` and `put` are not realistic. We cannot expect our program
to give us access to a reference to the entire `RealWorld`? What would we do
with it?  Furthermore, we shouldn\'t be able to change out the entire
`ReadWorld` with a new version (assuming we could construct one in the first
place).

Instead, the IO monad gives us access to *part* of the real world. For
example, the IO monad can give us access to a part of the computer\'s memory,
such as a mutable reference or mutable array.

The `Data.Vector.Mutable` library provides support for working with mutable
arrays in Haskell using an interface that looks somewhat like the following:

~~~~~~~~~~{.haskell}
    type MVector e   -- type of arrays containing elements of type e

    read       :: MVector e -> Int -> IO e
    write      :: MVector e -> Int -> e -> IO ()

    replicate  :: Int -> e -> IO (MVector e)
    replicateM :: Int -> IO e -> IO (MVector e)
    length     :: MVector e -> Int
~~~~~~~~~~

You can think of a `MVector` as a description of the part of the real world
that we want to look at.  In that case, `read` and `write` look a lot like
`get` and `put`.  The only difference is we need to say what part of the store
we want to use with get and put (i.e\. the `MVector`); we aren\'t working with
the entire store.

The `replicate` operation allows us to construct vectors (i.e\. define a part
of the RealWorld that we want access to). The `Int` is the length of the
vector, and the `e` argument is used to provide an initial value to all of the
arguments of the vector.

For example, the code below constructs a new vector of length 100, prints out
its length, writes to a particular location in that vector, and then accesses
the data from that same location. The analogous Java code for each line
is in the comments.

> example = do
>    v <- V.replicate 100 Nothing      -- Char[] v = new Char[100];
>    putStrLn (show (V.length v))      -- System.out.println(v.length);
>    V.write v 10 (Just 'a')           -- v[10] = 'a';
>    x <- V.read v 10                  -- Int x = v[10];
>    putStrLn (show x)                 -- System.out.println(v[10]);

Other than Haskell\'s lack of convenient notation for array access and update,
the two versions are comparable.

We can also create multidimensional arrays as vectors of vectors.

Initialize a 2-D array of size n m with 0s.  (The commented type is not the
real type of the function, but a useful fiction for now.)

> -- make_matrix :: Int -> Int -> IO (MVector (MVector Int))
> make_matrix n m =
>   V.replicateM n (V.replicate m 0)

Read from location i j in the vector.

> -- read_matrix :: MVector (MVector Int) -> Int -> Int -> IO Int
> read_matrix vec i j = do
>   m0 <- V.read vec i
>   V.read m0 j

Write the value x at location i j in the vector.

> -- write_matrix :: MVector (MVector Int) -> Int -> Int -> Int -> IO ()
> write_matrix vec i j x = do
>   m0 <- V.read vec i
>   V.write m0 j x

As a larger example, we can construct 2-dimensional arrays and use them to
solve the classic dynamic programming problem of [making
change](https://en.wikipedia.org/wiki/Change-making_problem).

*Note:* `forM_ :: Monad m => [a] -> (a -> m b) -> m ()` is `mapM_` with its arguments flipped.

> -- given a list of coin values, and a total amount,
> -- determine the smallest number of coins that can be used
> -- to make change for that amount.
> make_changeM :: [Int] -> Int -> IO Int
> make_changeM coins n = do
>   -- initialize the change making matrix
>   let num_coins = length coins
>   m <- make_matrix (num_coins + 1) (n + 1)
>   forM_ [ 0 .. n ] $ \i -> do
>      write_matrix m 0 i i
>   -- go through each of the coin values (and their indices)
>   forM_ (zip coins [ 1 .. num_coins ]) $ \(c,ci) ->
>     forM_ [ 1 .. n ] $ \r -> do
>        if (c == r) then
>           -- Use that coin
>           write_matrix m ci r 1
>        else if (c > r) then do
>           -- We use the previous solution for making r,
>           -- excluding coin c
>           v <- read_matrix m (ci-1) r
>           write_matrix m ci r v
>        else do
>         -- We can use c
>         -- We need to decide which one of the following solutions is the best:
>         -- 1. Using the previous solution for making r
>         --     (without using c)
>         -- 2. Using the previous solution for making r - c
>         --     (without using c) plus this 1 extra coin.
>         -- then write that answer at position ci
>           v1 <- read_matrix m (ci - 1) r
>           v2 <- read_matrix m ci (r - c)
>           write_matrix m ci r (min v1 (1 + v2))
>   read_matrix m num_coins n

Local State and the ST monad
----------------------------

One thing to notice about `makeChangeM` above is that all of the state that is
use by this function is *local* to the function. It needs to allocate the 2D
array, but once the answer is calculated, this array does not persist past the
execution of this function.

In this case, it is somewhat annoying that the type of `makeChangeM` has `IO`
in it. We should be able to treat it like a pure functional program.

It turns out that Haskell\'s type system is powerful enough to allow us to do
just that.  The library that we have been using is actually more general than
I\'ve been showing you. The `MVector` type in this file is really an
abbreviation for a more general type that mentions the \"store\" that the vector
is defined within.

~~~~~{.haskell}
      type MVector e = V.MVector RealWorld e
~~~~~~~~~~

When we use vectors from the IO monad, all we know about them is that they
come from the real world.

However, we can also use mutable vectors in the `ST` monad, which tracks the
part of the state that they reference. For example, we could also give
`make_matrix` above the following type:

> -- make_matrix :: Int -> Int -> ST s (V.MVector s (V.MVector s Int))

Furthermore, the `make_changeM` matrix also has this intriguing type~

> -- make_changeM :: [Int] -> Int -> ST s Int

*UPDATE THE TYPE OF make_changeM to this type RIGHT NOW*.

Types like this are where the Haskell `ST` monad really shines. This type
tells us that the function only uses *local state* and that the state doesn\'t
escape elsewhere into the computation. How do we know that looking at the
type? Because the type variable `s` only appears once, in the result type
of the function. It isn\'t part of any of the arguments to `make_changeM` nor
is it part of the final result type (i.e\. the last parameter to `ST`).

Because we know that this state can be isolated, we can use the `ST` monad
function below to run the computation to produce a pure value.

~~~~~{.haskell}
        runST :: (forall s. ST s a) -> a
~~~~~~~~~~~~

 What this means is that if you use state in your implementation to make it
faster, as long as your function is actually a function to the outside world,
it doesn\'t need to stay in the `ST` monad. (Whoa!  This is a function that we
*cannot* write in Haskell on our own.)

For example, once you change the type signature to `make_changeM`, you can
replace `undefined` below with `(make_changeM coins n)`.

> make_change :: [Int] -> Int -> Int
> make_change coins n = runST undefined

Note that we have to isolate the state. If we allow `s` to escape through the
result type of the ST computation

> -- bad = runST (make_matrix 10 10)

Or through some variable in the context that the computation refers to

> {-
> bad = do
>   m <- make_matrix 10 10
>   let y = runST (read_matrix m 5 5)
>   return y
> -}

we will get a type error error. Try uncommenting these lines, but don\'t read
the errors too closely \-\- they refer to concepts that we have not yet
covered.