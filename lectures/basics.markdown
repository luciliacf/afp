---
title: Basics
author: Lucilia FIgueiredo
---

The source code for this lecture can be dowloaded [here](../code/Lec01.hs). 

Every Haskell file begins with a few lines naming the module (this name must start with a capital letter and be the same as the file name) and (optionally) importing definitions from other modules.

~~~ {.haskell}
module Lec1 where      -- comments can begin with two dashes
import Test.HUnit      -- library imports must come at the beginning
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To load this file into ghci, you will need to install the `HUnit`
library first, by running  `cabal install HUnit`. 

Functional programming means that the semantics of a program can be described mathematically. One principle of mathematics is called Leibniz equality: in any context, we can replace an object with something equal to it. Therefore, in Haskell, we reason about computation by reasoning about the equality of (sub-)programs.

So, if we want to know the value of an arithmetic expression, we only need to find some number that is equal to it.

     3 * (4 + 5) 

  { by addition, 4+5 is equal to 9, so we can replace it }

     3 * 9

  { by multiplication }

     27 

That\'s it!

We can give names to Haskell expressions:

~~~~ {.haskell}
> x = 3 * (4 + 5)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

and ask ghci to calculate their values, just as we did above.


## What is Abstraction?

Pattern Recognition

~~~~ {.haskell}
31 * (42 + 56)

70 * (12 + 95)

90 * (68 + 12)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We recognize a pattern in the above expressions that we can generalize to a function by defining an equation

~~~~ {.haskell}
pat x y z = x * (y + z)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The important question is not \"What does this function do?\" but,
instead \"What does this function mean?\" 
We can reason about that meaning using what we know about equality.

    pat 31 42 56

 { function call }

    31 * (42 + 56)

 { addition }

    31 * 98

 { multiplication }

    3038

Functions, like  `pat`, are the core abstraction mechanisms in functional programming.


# Elements of Haskell

 * Everything is an expression
 * Expressions evaluate to values
 * Every expression has a type

## Basic types

It is good style to annotate the type of every declaration in a Haskell program.

~~~~ {.haskell}
i :: Int
i = 31 * (42 + 56)         -- word-sized integers

ii :: Integer
ii = 31 * (42 + 56)        -- arbitrarily large integers

d :: Double
d = 3.1 * (42 + 5)         -- double precision floating point
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(Note that numeric constants, `+`, and `*` are overloaded. Type annotations resolve ambiguity.)

One can also annotate the type of the expression directly.

~~~~ {.haskell}
c = 'a'    :: Char         -- characters
s = "abcd" :: String       -- strings
b = True   :: Bool         -- boolean values
u = ()     :: ()           -- 'unit' (both type and constant have same syntax)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Function types
 
    `a -> b`

Function taking an input of type `a` and yielding an output of type `b`

~~~~ {.haskell}
pos :: Int -> Bool
pos x = x > 0
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Multi-argument function types

    `a1 -> a2 -> a3 -> b`

Function taking inputs of type `a1`, `a2`, and `a3` and returning a result of type `b`

~~~~ {.haskell}
arith :: Int -> Int -> Int -> Int
arith x y z = x * (y + z)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Symbolic vs. alphabetic names

Symbolic identifiers (i.e. `+` and `*`) are infix by default.

Parens around a symbolic name turn it into a regular name.

~~~~ {.haskell}
plus :: Int -> Int -> Int
plus = (+)
p0 :: Int
p0 = (+) 2 ((*) 3 4)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Likewise we can use alphabetic name in backquotes as infix.

~~~~ {.haskell}
p1 :: Int
p1 = 2 `plus` 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Making Haskell DO something

Programs often interact with the world:

* Read files
* Display graphics
* Broadcast packets

They don\'t just compute values.

How does this fit with values and equalities above?

Note, we\'ve gotten far without doing any I/O. That\'s fairly standard
in Haskell. Working with GHCi means that we can see the answers
directly, we don\'t need an action to print them out. However, a
standalone executable needs to do something, so we demonstrate that here.

## I/O via an \"Action\" Value

**"IO actions"** are a new sort of sort of value that describe an effect on the world.

    `IO a`   --  Type of an action that returns an  \`a\`  (a can be anything!) 


## Obligatory Hello World

Actions that do something but return nothing have the type `IO ()`.

~~~~ {.haskell}
putStr :: String -> IO ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

So `putStr` takes in a string and returns an action that writes
the string to stdout.

GHCi can execute actions interactively.

~~~~ {.haskell}
hw :: IO ()
hw = putStr "Hello World! \n"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ghci> hw

Alternatively, we can compile our program as an executable and run it. 

The only way to \"execute\" the action (without using ghci), is to make
it the value of name `main`.

~~~~ {.haskell}
main :: IO ()
main = hw
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The batch compiler \"ghc\" compiles and run large programs. There must
be a definition of  `main` somewhere.

(Note: There can also be multiple source files in a Haskell
application,  and if the one that includes `main`  is called
`Main.hs` you can leave off the `-main-is` flag. There are
also much more sophisticated ways to manage the compilation of Haskell
applications, especially those built from libraries and multiple source files.)

## Just \'do\' it

How can we do many actions? By composing small actions.

The do syntax allows us to create a compound action that sequences one
action after another. The definition of many below is a compound
action that  outputs the three strings in order. 

~~~~ {.haskell}
many :: IO ()
many = do putStr "Hello"     -- each line in the sequence
          putStr " World!"   -- must be an IO action
          putStr "\n"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note: white-space is significant here. The do notation sequences
 actions, but each action in the sequence must start at the same
 character offset.

Sometimes people put the do on a line by itself and then start the
list of actions on the next line.

~~~~ {.haskell}
many' :: IO ()
many' = do
   putStr "Hello"
   putStr " World!"
   putStr "\n"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Example: Input Action

Actions can also return a value.

~~~~ {.haskell}
getLine :: IO String
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
This action reads and returns a line from stdin. 
We can name the result as part of a do sequence, with this notation

~~~~ {.haskell}
x <- action
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
Here `x`  is a variable that can be used to refer to the result of
the action in later code.

~~~~ {.haskell}
query :: IO ()
query = do 
   putStr "What is your name? "
   n <- getLine
   let y :: String
       y = "Welcome to Advanced FP " ++ n
   putStrLn y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note, when we sequence actions of type `IO ()` there is no need to
name the result. These actions do not return interesting results. 
We could name the result if we wanted (such as `m` below); but because
of its type we know that `m` will always be `()`.

~~~~ {.haskell}
query' :: IO ()
query' = do 
   m <- putStr "What is your name? "
   n <- getLine
   putStrLn ("Welcome to Advanced FP " ++ n)
   st <- query2
   return ()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note that you cannot name the last action in a sequence. The name is
there so that you can use the result later. 
If you want to return the value instead, the last action should be a `return`.

~~~~ {.haskell}
query2 :: IO String
query2 = do putStr "What is your name? "
            n <- getLine
            return n
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There is no need to name a value if it is just going to be
returned. This version is equivalent.

~~~~ {.haskell}
query2' :: IO String
query2' = do putStr "What is your name? "
             getLine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Example: Testing Actions

The [**HUnit**][HUnit] library contains definitions for constructing
unit tests for your programs. To use this library you must first install it with
the cabal tool. This library defines the `Test` type for test cases.

~~~~ {.haskell}
t1 :: Test
t1 = 3 ~?= 1 + 2     -- check that the expected value `3`
                     -- matches the result of the computation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To run the test case, we need to use the function `runTestTT :: Test -> IO Counts`
	   
~~~~ {.haskell}
numTest :: IO Counts
numTest = runTestTT t1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is an action that runs the test case(s) and returns a data
structure recording  which ones pass and fail.

~~~~ {.haskell}
dotest :: IO ()
dotest = do c <- runTestTT (3 ~?= 3)
            print c
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

[HUnit]: https://hackage.haskell.org/package/HUnit


Reading
--------

  * Miran Lipovaca, *Learn You a Haskell for Great Good!*, chapters 1
    to 4.
	
  * Graham Hutton, *Programming in Haskell* , 2nd Ed., chapters 1 to 2.
