---
title: Higher-Order Programming Patterns
author: Lucilia FIgueiredo
--- 

The source code for this lecture can be dowloaded [here](../code/Lec03.hs). 

~~~~~ {.haskell}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Lec3 where
import Prelude hiding (map, foldr, filter, pred, sum, product)
import Data.Char
import Test.HUnit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Functions Are Data

As in all functional languages, Haskell functions are first-class
values, meaning that they can  be treated just as you would any other data.

You can pass functions around in any manner that you can pass any
other data around.  For example, suppose you have the simple functions
`plus1` and  `minus1` defined via the equations:

~~~~~ {.haskell}
plus1 :: Int -> Int
plus1 x = x + 1

minus1 :: Int -> Int
minus1 x = x - 1
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, you can make a pair containing both the functions

~~~~~ {.haskell}
funp :: (Int -> Int, Int -> Int)
funp = (plus1, minus1)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or you can make a list containing the functions

~~~~~ {.haskell}
funs :: [Int -> Int]
funs = undefined  -- fill in a definition here
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Taking Functions as Input

This innocent looking feature makes a langage surprisingly brawny and
flexible, because now, we can write higher-order functions that take
functions as input and return functions as output! Consider:

~~~~~ {.haskell}
doTwice :: (a -> a) -> a -> a
doTwice f x = f (f x)

dtTests :: Test
dtTests = TestList [ doTwice plus1  4 ~?= 6, doTwice minus1 5 ~?= 3 ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Here, ```doTwice``` takes two inputs: a function `f` and value
`x`,  and returns the the result of applying `f` to `x`,
and  feeding that result back into `f` to get the final
output. Note how the  raw code is clearer to understand than my
long-winded  English description!

Last time we talked about how programs execute in Haskell: we just
substitute equals for equals.  Let\'s think about an example with `doTwice`:

    doTwice plus1 10 == plus1 (plus1 10)        {- unfold doTwice -}
                     == plus1 (10 + 1)          {- unfold plus1 -}
                     == (10 + 1) + 1            {- unfold plus1 -}
                     == 12                      {- old-school arithmetic -}

## Returning Functions as Output

Similarly, it can be useful to write functions that return new
functions as output.  For example, rather than writing different
versions `plus1`, `plus2`, `plus3`  etc. we can just write
a single function `plusn`  as

~~~~~ {.haskell}
plusn :: Int -> (Int -> Int)
plusn n = f
    where f x = x + n
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

That is, `plusn` returns as output a function which itself takes
as input an integer `x` and adds n to it. Lets use it

~~~~~ {.haskell}
plus10  :: Int -> Int
plus10  = plusn 10

minus20 :: Int -> Int
minus20 = plusn (-20)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Note the types of the above are `Int -> Int`. That is, `plus10` and
`minus20` are functions that take in an integer and return an
integer  (even though we didn\'t explicitly give them an argument).
Try it out in GHCi:

    ghci> plus10 3
	13
    ghci> plusn 10 3
	13

# Partial Application

In regular arithmetic, the `-` operator is left-associative. Hence,

    2 - 1 - 1 == (2 - 1) - 1 == 0
	
(and not `2 - (1 - 1) == 2` !). Just like `-` is an arithmetic operator
that takes two numbers and returns an number, in Haskell, `->` is
a type operator that  takes two types, the input and output, and
returns a new function type. However, operator `->`  is right-associative: the type
`Int -> Int -> Int`  is equivalent to  `Int -> (Int -> Int)`.
Equipped with this knowledge, consider the function:

~~~~~ {.haskell}
plus :: Int -> Int -> Int
plus m n = m + n
~~~~~~~~~~~~~~~~~~~~~

Whenever we use `plus` we can either pass in both the inputs at
once,  as in `plus 10 20`, or instead, we can partially apply the
function,  by just passing in only one  input out of the two that it expects.

~~~~~ {.haskell}
plusfive :: Int -> Int
plusfive = plus 5
~~~~~~~~~~~~~~~~~~~~~

thereby getting as output a function that is waiting for the second
input  (at which point it will produce the final result).

~~~~~ {.haskell} 
pfivetest :: Test
pfivetest = plusfive 1000 ~?= 1005
~~~~~~~~~~~~~~~~~~~~~~

So how does this execute? Again substitute equals for equals

    plusfive 1000 == plus 5 1000       {- definition of plusfive -}
                  == 5 + 1000          {- unfold plus -}
                  == 1005              {- arithmetic -}

If you have been following so far, you should know how this behaves.

~~~~~ {.haskell} 
doTwicePlus20 :: Int -> Int
doTwicePlus20 = doTwice (plus 20)
~~~~~~~~~~~~~~~~~~~~~~

First, see if you can figure out the type. Next, see if you can figure out how this evaluates.

    doTwicePlus20 0 == doTwice (plus 20) 0
                    == (plus 20) ((plus 20) 0)
                    == ...  (fill this part in) ... == 20 + 20 + 0 == 40

# Anonymous Functions

As we have seen, with Haskell, it is quite easy to create function
values that are  not bound to any name. For example the expression
plus 1000 yields a  function value that is not bound to any name.

We will see many situations where a particular function is only used
once,  and hence, there is no need to explicitly name it. Haskell
provides a  mechanism to create such anonymous functions. For example,

    \x -> x + 1

is an expression that corresponds to a function that takes an argument
`x` and  returns as output the value `x + 1`. The function has no
name,  but we can use it in the same place where we would write a function.

~~~~~ {.haskell} 
anonTests :: Test
anonTests = TestList [ (\x -> x + 1) 100 ~?= 101, doTwice (\x -> x + 1) 100 ~?= 102 ]
~~~~~~~~~~~~~~~~~~~

Of course, we could name the function if we wanted to

~~~~~ {.haskell}
plus1' :: Int -> Int
plus1' = \x -> x + 1
~~~~~~~~~~~~~~~~~~~

Indeed, in general, a function defining equation

    f x1 x2 ... xn = e

is equivalent to

    f = \x1 x2 ... xn -> e	 
	
# Infix Operations and Sections

In order to improve readability, Haskell allows you to use certain
functions as  infix operations: a function whose name appears in
parentheses  can be used as an infix operation. My personal favorite
infix operator is  the application function, defined like this:

~~~~~ {.haskell}
($) :: (a -> b) -> a -> b
f $ x = f x
~~~~~~~~~~~~~~~~~~~~~~

Huh? Doesn\'t seem so compelling does it? It\'s just application.
Actually, its very handy because it has different precedence than
normal  application. For example, I can write:

    minus20 $ plus 30 32

Which means the same as:

    minus20 (plus 30 32)

That is, Haskell interprets everything after the `$` as one argument to
`minus20`.  I couldn\'t do this by writing

    minus20 plus 30 32    --- WRONG!
	
because Haskell would think this was the application of `minus20`
to the  three separate arguments `plus`, `30` and `32`.

We will see many infix operators in the course of the class; indeed we
have already seen some defined in the standard prelude. For example

~~~~~ {.haskell}
(:) :: a -> [a] -> [a]
~~~~~~~~~~~~~~~~~~~~~~

as well as the arithmetic operators `(+)`, `(*)` and `(-)`.

Recall also that Haskell allows you to use any function as an infix
operator,  simply by wrapping it inside backticks.

~~~~~ {.haskell}
anotherFive :: Int
anotherFive = 2 `plus` 3
~~~~~~~~~~~~~~~~~~~

To further improve readability, Haskell allows you to use partially
applied infix  operators, ie. infix operators with only a single
argument. These are called sections. Thus, the section `(+1)` is
simply a function that takes as input a number, the argument missing
on the left  of the `+` and returns that number plus `1`.

~~~~~ {.haskell}
anotherFour :: Int
anotherFour = doTwice (+2) 0
~~~~~~~~~~~~~~~~~~~

Similarly, the section `(1:)` takes a list of numbers and returns a
new list with `1`  followed by the input list. So `doTwice (1:)
[2..5]`  evaluates to ```[1,1,2,3,4,5]```.

For practice, define the singleton operation as a section, so that the
following test passes.

~~~~~ {.haskell}
singleton :: a -> [a]
singleton = undefined   -- fill in your definition

singletonTest :: Test
singletonTest = singleton True ~?= [True]
~~~~~~~~~~~~~~~~~~~

# Polymorphism

We used to `doTwice` to repeat an arithmetic operation, but the
actual body  of the function is oblivious to how `f` behaves.
We say that `doTwice` is polymorphic, in the sense that it works
with  different types  of values, eg functions that increment integers
and  concatenate strings.  This is vital for abstraction. The general
notion of repeating, ie doing twice  is entirely independent from the
types  of the operation that is  being repeated, and so we shouldn\'t
have to write separate repeaters  for integers and strings. Polymorphism
allows  us to reuse the same abstraction `doTwice` in different settings.

Of course, with great power, comes great responsibility.
The section `(10 <)` takes an integer and returns `True` iff
the  integer is greater than `10`.

~~~~~ {.haskell}
greaterThan10 :: Int -> Bool
greaterThan10 = (10 <)
~~~~~~~~~~~~~~~~~~~

However, because the input and output types are different, it doesn\'t
make sense  to try `doTwice greaterThan10`. A quick glance at the
type of `doTwice`  would tell us this:

    doTwice :: (a -> a) -> a -> a
	
The `a` above is a type variable. The signature above states that
the first  argument to `doTwice` must be a function that maps
values of type `a` to `a`, i.e.,  must produce an output that
has  the same type as its input (so that that output can be fed into
the function again!).  The second argument must also be an `a` at which
point we are guaranteed that the result from `doTwice` will also be
an `a`.  The above holds for any `a` which allows us to safely re-use
`doTwice` in  different settings.

Of course, if the input and output type of the input function are
different,  as in `greaterThan10`, then the function is incompatible with `doTwice`.
Ok, to make sure you\'re following, can you figure out what this does?

~~~~~ {.haskell}
ex1 :: (a -> a) -> a -> a
ex1 = doTwice doTwice

ex1Test :: Test
ex1Test = undefined  -- fill in your definition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Polymorphic Data Structures

Polymorphic functions that can operate on different kinds of values
are  often associated with polymorphic data structures that can
contain  different kinds of values. The types of such functions and
data  structures are written with one or more type variables.

For example, the list length function:

~~~~~ {.haskell}
len :: [a] -> Int
len []     = 0
len (_:xs) = 1 + len xs
~~~~~~~~~~~~~~~~~~~

The function\'s type states that we can invoke `len` on any kind of list.
The type variable `a` is a placeholder that is replaced with the actual
type of the list elements at different application sites. Thus, in
the following  applications of `len`, `a` is replaced with
`Double`,  `Char` and `[Int]` respectively.

~~~~~ {.haskell}
len [1.1, 2.2, 3.3, 4.4] :: Int
len "mmm donuts!"  :: Int
len [[], [1], [1,2], [1,2,3]] :: Int
~~~~~~~~~~~~~~~~~~~

Most of the standard list manipulating functions, for example those in
the  standard library `Data.List`,  have generic types. With a
little practice, you\'ll  find that the type signature contains a
surprising amount of  information about how the function behaves.

In particular, note that we cannot \"fake\" values of generic types. For
example,  try to replace the undefined below with a result that
doesn\'t throw an  exception (like undefined does) or go into an
infinite loop.  (NB: Using a function that starts with unsafe doesn\'t count.)

~~~~~ {.haskell}
impossible :: a
impossible = undefined
~~~~~~~~~~~~~~~~~~~

Because `impossible` has to have any type, there is no real value that
we can  provide for it.

This reasoning extends to other types too. For example, the generic
type of the `const` function

~~~~~ {.haskell}
const :: a -> b -> a
~~~~~~~~~~~~~~~~~~~
tells us that the output of this function (if there is any) must be
the first argument.  There is no other way to produce a generic result
of type `a`.  (And the second argument must be completely ignored,
there  is no way to use it in a generic way.)

# \"Bottling\" Computation Patterns With Polymorphic Higher-Order Functions
   
The combination of polymorphism and higher-order functions is the
secret  sauce that makes FP so tasty. It allows us to take patterns of
computation  that reappear in different guises in different places,
and crisply  specify them as reusable strategies. Let\'s look at some
concrete  examples.

## Computation Pattern: Iteration 

Let\'s write a function that converts a string to uppercase. Recall
that,  in Haskell, a String is nothing but a list of Chars. So
we must  start with a function that will convert an individual Char to
its  uppercase version. Once we find this function, we will simply
walk over  the list, and apply the function to each Char.

How might we find such a transformer? Let\'s query **Hoogle** for a
function of the  appropriate type! Ah, we see that the module
`Data.Char` contains  such a function:

~~~~~ {.haskell}
toUpper :: Char -> Char
~~~~~~~~~~

Using this, we can write a simple recursive function that does what we need:

~~~~~ {.haskell}
toUpperString :: String -> String
toUpperString [] = []
toUpperString (x:xs) = toUpper x : toUpperString xs
~~~~~~~~~~~~~~~~~~~~~~~~~~

This pattern of of recursion appears all over the place. For example,
suppose we represent a location on the plane using a pair of Doubles
(for the x \- and y \- coordinates)  and we have a list of points that represent a polygon.

~~~~~ {.haskell}
type XY      = (Double, Double)
type Polygon = [XY]
~~~~~~~~~~~~~~~~~~~~~~~~~~

It\'s easy to write a function that shifts a point by a specific amount:

~~~~~ {.haskell}
shiftXY :: XY -> XY -> XY
shiftXY (dx, dy) (x, y) = (x+dx,y+dy)
~~~~~~~~~~~~~~~~~~~~~~~~~~

How would we translate a polygon? Just walk over all the points in the
polygon  and translate them individually.

~~~~~ {.haskell}
shiftPoly :: XY -> Polygon -> Polygon
shiftPoly _ []       = []
shiftPoly d (xy:xys) = shiftXY d xy : shiftPoly d xys
~~~~~~~~~~~~~~~~~~~~~~~~~~

Now, some people (using some languages) might be quite happy with the
above code. But what separates a good programmer from a great one is
the  ability to abstract.

The functions `toUpperString` and `shiftPoly` share the same
computational  structure: they walk over a list and apply a function
to each element.  We can abstract this common pattern out as a
higher-order function, `map`.  Since the two functions we\'re
abstracting  differ only in what they do to each list element, so
we\'ll  just take that as an input!

~~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs
~~~~~~~~~~~~~~~~~~~

The type of `map` tells us exactly what it does: it takes an `a ->b` transformer 
and list  of `a` values, and transforms each `a` value to
return a list of `b`  values. We can now safely reuse the pattern,
by instantiating  the transformer with different specific operations.

~~~~~ {.haskell}
toUpperString' :: String -> String
toUpperString' xs = map toUpper xs

shiftPoly' :: XY -> Polygon -> Polygon
shiftPoly' d = undefined
~~~~~~~~~~~~~~~~~~~

Much better. But let\'s make sure our refactoring didn\'t break anything!

~~~~~ {.haskell}
testMap = runTestTT $ TestList $ 
              [ toUpperString' "abc" ~?= toUpperString "abc",
                shiftPoly' (0.5,0.5) [(1,1),(2,2),(3,3)] ~?= shiftPoly (0.5,0.5) [(1,1),(2,2),(3,3)] ]
~~~~~~~~~~~~~~~~~~~

By the way, what happened to the list parameters of
`toUpperString` and `shiftPoly`?  Two words: partial
application. In general, in Haskell, a function definition equation
`f x = e x `  is identical to ` f = e ` as long as ` x`
isn\'t  used in `e`. Thus, to save ourselves the
trouble  of typing, and the blight of seeing the vestigial `x`,
we often prefer to just leave it out altogether.
(As an exercise, you may like to prove to yourself using just
equational reasoning,  using the equality laws we have seen, that the
above  versions of `toUpperString` and `shiftPoly`  are equivalent.)

We\'ve already seen a few other examples of the `map` pattern. Recall the
`listIncr` function, which added `1` to each element of a list:

~~~~~ {.haskell}
listIncr :: [Int] -> [Int]
listIncr []     = []
listIncr (x:xs) = (x+1) : listIncr xs
~~~~~~~~~~~~~~~~~~~

We can write this more cleanly with `map`, of course:

~~~~~ {.haskell}
listIncr' :: [Int] -> [Int]
listIncr' = undefined
~~~~~~~~~~~~~~~~~~~

## Computation Pattern: Folding

Once you\'ve put on the FP goggles, you start seeing a handful of
computation  patterns popping up everywhere. Here\'s another.

Let\'s write a function that adds all the elements of a list.

~~~~~ {.haskell}
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + (sum xs)
~~~~~~~~~~~~~~~~~~~

Next, a function that multiplies all the elements of a list.

~~~~~ {.haskell}
product :: [Int] -> Int
product [] = 1
product (x:xs) = x * (product xs)
~~~~~~~~~~~~~~~~~~~

Can you see the pattern? Again, the only bits that are different are
the base  case value, and the function being used to combine the list
element  with the recursive result at each step. We\'ll just turn those
into  parameters, and lo!

~~~~~ {.haskell}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f base []     = base
foldr f base (x:xs) = x `f` (foldr f base xs)
~~~~~~~~~~~~~~~~~~~

Now, each of the individual functions are just specific instances of
the general `foldr` pattern.

~~~~~ {.haskell}
sum', product' :: [Int] -> Int
sum'     = foldr (+) 0
product' = foldr (*) 1

foldrTest = runTestTT $ TestList [sum' [1,2,3] ~?= sum [1,2,3], product' [1,2,3] ~?= product [1,2,3] ]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To develop some intuition about `foldr` let\'s unfold an example a few times by hand.

    foldr f base [x1,x2,...,xn]
      == f x1 (foldr f base [x2,...,xn])           {- unfold -}
      == f x1 (f x2 (foldr f base [...,xn]))       {- unfold -}
      == x1 `f` (x2 `f` ... (xn `f` base))

Aha! It has a rather pleasing structure that mirrors that of lists;
the `:`  is replaced by the `f` and the `[]` is replaced
by base. So can you see how to use it to eliminate recursion from the
recursion  from our list-length function?

~~~~~ {.haskell}
len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

len' :: [a] -> Int
len' = undefined    -- fill in your definition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Or, how would you use `foldr` to eliminate the recursion from this?

~~~~~ {.haskell}
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

factorial' :: Int -> Int
factorial' n = undefined  -- fill in your definition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

OK, one more. The standard list library function `filter` has this type:

~~~~~ {.haskell}
filter :: (a -> Bool) -> [a] -> [a]
~~~~~~~~~~~~~~~~

The idea is that it the output list should contain only the elements
of the  first list for which the input function returns True. So:

~~~~~ {.haskell}
filterTests :: Test
filterTests = TestList [ filter (>10) [1..20] ~?= [11..20],
                         filter (\l -> sum l <= 42) [ [10,20], [50,50], [1..5] ] ~?= [[10,20],[1..5]] ]
~~~~~~~~~~~~~~~~~~~								
																		
Can we implement `filter` using  `foldr? `?  Sure!

~~~~~ {.haskell}
filter pred = undefined  -- fill in your definition
runFilterTests = runTestTT filterTests
~~~~~~~~~~~~

## Which is more readable? HOFs or Recursion

As a beginner, you might find the explicitly recursive versions of
some of  these functions easier to follow than the map and foldr
versions.  However, as you write more Haskell, you will probably start
to find the latter  are far easier, because map and foldr encapsulate
such common  patterns that you\'ll become completely accustomed to
thinking in  terms of them and other similar abstractions.

In contrast, explicitly writing out the recursive pattern matching
should  start to feel needlessly low-level. Every time you see a
recursive function,  you have to understand how the knots are tied \-\-
and worse,  there is potential for making silly off-by-one type errors
if you re-jigger the basic strategy every time.

As an added bonus, it can be quite useful and profitable to
parallelize and  distribute the computation patterns (like map and
foldr) in  just one place, thereby allowing arbitrary hundreds or
thousands  of instances to benefit in a single shot!.

We\'ll see some other similar patterns later on.

Reading
--------

  * Miran Lipovaca, *Learn You a Haskell for Great Good!*, chapter 5.
	
  * Graham Hutton, *Programming in Haskell* , 2nd Ed., chapters 6 and 7..
