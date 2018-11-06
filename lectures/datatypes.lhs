---
title: User-defined datatypes
author: Lucilia Figueiredo
---

[datatypeslhs]: datatypes.lhs
[datatypes-sol]: datatypes-sol.html

*Note:* this is the stubbed version of module Datatypes. You may download the [lhs version][datatypeslhs]
of this module and replace all parts marked undefined. Eventually, the [complete version][datatypes-sol] will
be made available.

> {-# OPTIONS -fno-warn-type-defaults -fwarn-incomplete-patterns #-}
> module Lec4 where
> import Prelude hiding (Maybe,Just,Nothing,Either,Left,Right)
> import Test.HUnit

So far, we\'ve mostly talked about how to use the types that appear in
the Haskell standard library.  We also discussed a few type synonyms,
like

~~~~~{.haskell}
type XY = (Double,Double)
~~~~~~~~~~~~

but we haven\'t described any ways to define really _new_ types.

As a motivating example, suppose you are writing an application that
deals with calendars, and you need to represent the days of the week.
You might be tempted to use `String` or `Int`, but both of these choices
have downsides.  If you use `type Day = String` there will be lots of `Day`s
that don\'t actually represent real days.
Also, you will need to devise and adhere to some sort of
standardization \- is Monday represented by \"Monday\", \"monday\",
\"MONDAY\", or \"Mon\"?  Should you handle more than one of these?

The choice `type Day = Int` has similar problems.  There are lots of Ints that won\'t represent
valid days.  And you\'ll have to remember whether you pick Sunday or
Monday to be the first day of the week, and whether it is represented
by 0 or 1.

Haskell has a better solution: user-defined datatypes.

> data Day = Monday
>          | Tuesday
>          | Wednesday
>          | Thursday
>          | Friday
>          | Saturday
>          | Sunday
>   deriving (Show, Eq)

The new values (`Monday`, `Tuesday`, etc.) are called \"constructors\" or \"data
constructors\".  This is a very simple example of a datatype (basically just an
enumeration), but we\'ll see more examples in a minute.

The last line enables printing and equality for this datatype. We\'ll
see more about this soon, but it is common to do this for every datatype.

We can define functions on datatypes by pattern matching!  For example:

> nextWeekday :: Day -> Day
> nextWeekday Monday    = Tuesday
> nextWeekday Tuesday   = Wednesday
> nextWeekday Wednesday = Thursday
> nextWeekday Thursday  = Friday
> nextWeekday Friday    = Monday
> nextWeekday Saturday  = Monday
> nextWeekday Sunday    = Monday

This is great.  Now we don\'t have to worry about the difference
between \"Monday\" and \"monday\" or which Int corresponds to which day.
If we make a typo (for example, write Frday instead of Friday), the
compiler will tell us _at compile time_.   If we forget to handle
one of the days in some function, the compiler will warn us about that
too (inexhaustive pattern match warning).

Let\'s write one more function on `Day`s, to compute when a package
will arrive by \"two day shipping\":

> twoBusinessDays :: Day -> Day
> twoBusinessDays d = undefined

Datatypes can carry data values, too.  For example, here is a datatype
for representing shapes:

> data Shape =
>    Circle    Double Double Double
>  | Rectangle Double Double Double Double deriving (Eq, Show)

Here, `Circle` and `Rectangle` are the constructors \- every `Shape`
value must be one or the other.  Each constructor takes some
arguments:

- A `Circle` is specified by three `Doubles`.  These represent the x
  and y coordinates of the center and the radius.

- A `Rectangle` is specifed by four `Doubles`.  The first two are
  the coordinates of the lower left corner, and the second two are the
  coordinates of the upper right corner.

We can pattern match on shapes.  For example, here is a function that
computes the area of any `Shape`:

> area :: Shape -> Double
> area (Circle x y r) = pi * r * r
> area (Rectangle llx lly urx ury) = width * height where
>     width  = urx - llx
>     height = ury - lly

Note that constructors are first-class Haskell values, and
\-\- like any value \-\- they have types.

For example the types of `Monday` and `Tuesday` shouldn\'t surprise you:

~~~~~{.haskell}
Monday  :: Day
Tuesday :: Day
~~~~~~~~~~~~~~~

The constructors that take arguments have _function_ types.  For
example, you must apply `Circle` to three `Double`s to get a `Shape`:

~~~~~{.haskell}
Circle    :: Double -> Double -> Double -> Shape

Rectangle :: Double -> Double -> Double -> Double -> Shape
~~~~~~~~~~

Records
=======

We can also give *names* to the arguments of data constructors, using
Haskell\'s records. For example, we can define a point in space as an x and y
coordinate.

> data Point = Point { x :: Double , y :: Double } deriving (Show, Eq)

> point1 = Point { y = 1.0, x = 2.0 } -- order doesn't matter

> point2 = Point 1.0 1.0     --- Be careful, can also leave field names off
>                            --- but here the order does matter

Each field name defines a *selector* for that component of the data
structure.

> x1 = x point1

When taking arguments that use records, we can either use the
record selectors, or pattern match.

> distFromOrigin :: Point -> Double
> distFromOrigin (Point {x = px, y = py}) = sqrt (px * px + py * py)

Rewrite this function using selectors `x` and `y`:

> distFromOrigin' p = undefined

Things to watch out for with records in Haskell:

 * Records must be defined as part of a datatype definition.

 * The selectors are first-class functions \-\- there is no such thing
   as \"r.x\".

 * Record selectors must be unique within a module. If `Point` has an `x`
   component, then no other datatype in that module can use `x`.

 * It\'s idiomatic to \"pun\" when pattern matching records. This can be
   confusing. For example, we name the variables using the same names as the
   selectors.

> distFromOrigin'' (Point {x = x, y= y}) = sqrt (x * x + y * y)

 * Records are purely functional in Haskell. There is no way to modify
   a component when it is created. However, there is an easy way to
   construct new values that share components with existing structures.

> point3 :: Point
> point3 = point1 { x = 2.0 }


Recursive Datatypes
===================

Datatypes can be defined recursively.  That is, their constructors can
take other elements of the same type as arguments.For example, we could define
a type representing *nonempty* lists of integers:

> data IntListNE = ISingle Int
>                | ICons Int IntListNE

So that the list `[1,2,3]` is represented as:

> oneTwoThree :: IntListNE
> oneTwoThree = ICons 1 (ICons 2 (ISingle 3))

For comparison with Haskell\'s built-in lists, it might help to think
of this as:

> oneTwoThree' :: IntListNE
> oneTwoThree' = 1 `ICons` (2 `ICons` ISingle 3)   --- backticks for infix

We can define functions by recursion on `IntListNE`s too, of course:

> sumOfIntListNE :: IntListNE -> Int
> sumOfIntListNE = undefined

> testSumIL = "sumOfIntListNE" ~: sumOfIntListNE oneTwoThree ~?= 6


Polymorphic Datatypes
=====================

It would sure be annoying to have a seperate kind of list for each
type of data!  Luckily, we know Haskell\'s list type is polymorphic:
you can have a list of type `[a]` for any `a`.

We can define new polymorphic datatypes too. For example, we can
easily make the non-empty lists above polymorphic.

As another example, here is the definition of the `Maybe` type (from the
Prelude) that we\'ve used in past lectures:

> data Maybe a = Nothing | Just a

Notice that the type `Maybe` itself takes an argument: the type
variable `a`.  We\'re also allowed to use that type variable in the
constructors.  So `Just` is a constructor that can be applied to
values of any type and will create a `Maybe` with the same type:

~~~~~{.haskell}
Just :: a -> Maybe a
~~~~~~~~~~

Thus, `Just` and `Nothing` work at any type:

> noInt :: Maybe Int
> noInt = Nothing

> justTrue :: Maybe Bool
> justTrue = Just True

> justThree :: Maybe Int
> justThree = undefined

A number of other polymorphic datatypes appear in the standard
library.  For example, here\'s a datatype that\'s useful when you
want to carry around values that could have either of two types:

> data Either a b = Left a | Right b

`Either` is often useful for error handling.  Sometimes returning a
`Maybe a` isn\'t quite good enough because you\'d like to give a helpful
error message.  `Either String a` works a little better: you can use
`Left msg` in the case of an error, and `Right v` in case things
are \.\.\. all right.

For example, here\'s a safer integer division function:

> safeDiv :: Int -> Int -> Either String Int
> safeDiv _ 0 = Left "You can't divide by zero, silly."
> safeDiv x y = Right $ x `div` y

Of course, Either is more useful when things can go wrong in more
than one way.


Trees
=====

Now let\'s play a bit with a bigger example: trees.  Here\'s one way to
define binary trees that have data at the internal nodes in Haskell:

> data Tree a = Empty    -- No data
>             | Branch a (Tree a) (Tree a)  -- data of type a, left and right subtrees
>                deriving (Eq, Show)

For example, we can represent the following tree

              5
            /   \
           2     9
          / \     \
         1   4     7

like this:

> exTree :: Tree Int
> exTree = Branch 5 (Branch 2 (Branch 1 Empty Empty) (Branch 4 Empty Empty))
>                   (Branch 9 Empty (Branch 7 Empty Empty))


We can write simple functions on trees by recursion:

> -- | increment all integers in the tree
> treePlus :: Tree Int -> Int -> Tree Int
> treePlus = undefined

> testTreePlus = "treePlus" ~: treePlus (Branch 2 Empty Empty) 3 ~?= Branch 5 Empty Empty


We can accumulate all of the elements in a tree into a list:

> infixOrder :: Tree a -> [a]
> infixOrder Empty = []
> infixOrder (Branch x l r) = infixOrder l ++ [x] ++ infixOrder r

> testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1,2,4,5,9,7]

\.\.\. visiting the nodes in different orders \.\.\.

> prefixOrder :: Tree a -> [a]
> prefixOrder = undefined

> testPrefixOrder = "prefixOrder" ~: prefixOrder exTree ~?= [5,2,1,4,9,7]

(NOTE: This is a simple way of defining a tree walk in Haskell, but it is not
the best way. In particular, the `infixOrder` function is *not* linear in the
number of nodes in the tree. Why?  Can you think of a way to rewrite
`infixOrder` so that it runs in linear time?)

> -- linear time infixOrder, using an accumulator and tail recursion
> infixOrder' :: Tree a -> [a]
> infixOrder' t = infixO t []
> infixO Empty xs = xs
> infixO (Branch x l r) xs = infixO l (x : (infixO r xs)

But, of course, what we should really do is reimplement our
higher-order patterns for trees!

> treeMap :: (a -> b) -> Tree a -> Tree b
> treeMap f Empty = Empty
> treeMap f (Branch x l r) = Branch (f x) (treeMap f l) (treeMap f r)

So that, for example, to increment each node in a `Tree Int` by one, we could
write this:

> treeIncr :: Tree Int -> Tree Int
> treeIncr = treeMap (+1)

> testTreeIncr = "treeIncr" ~: treeIncr (Branch 1 (Branch 2 Empty Empty) Empty) ~?=
>                                        Branch 2 (Branch 3 Empty Empty) Empty

> main :: IO ()
> main = do
>   runTestTT $ TestList [
>     testSumIL,
>     testTreePlus,
>     testInfixOrder,
>     testTreeIncr,
>     testPrefixOrder ]
>   return ()

Reading
--------
	
  * Graham Hutton, *Programming in Haskell* , 2nd Ed., chapter 3.

  * Miran Lipovaca, [*Learn You a Haskell for Great Good*](http://learnyouahaskell.com/), chapter 7

**Further reading**:

  * [The inner beauty of tree traversals (in Haskell)][treeTraversal]

[treeTraversal]: http://blog.moertel.com/posts/2012-01-26-the-inner-beauty-of-tree-traversals.html
