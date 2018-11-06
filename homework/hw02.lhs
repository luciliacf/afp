---
title: Homework 02: List Processing and recursion
date: September 2018
---

[hw02zip]: ../code/hw02.zip
[hw02-sol]: ../code/hw02-sol.zip
[lists]:    ../lectures/lists.html
[cwebpage]: 

This homework assignment provides practice with the basic built-in data structures of Haskell,
including lists, tuples and maybes, as well as recursion and pattern matching ([_Lec2_][lists]).
It also covers the basics of code style and test-driven development.

To complete your assignment, download the [zipfile][hw02zip] and edit `Main.hs`
(this file contains just the code) and submit it through the [course web page][cwebpage].

A solution to this homework will eventually be available [here][hw02-sol]

Before we go any further, you should make sure that you can compile and run this code.
Load the it interactively into ghci. You can then type main to run the main routine.
Alternatively, to compile the code from the command line type

    ghc --make Main
 
which will produce an executable (named `Main`, `a.out` or `Main.exe` depending on your system).

The first line of actual code for this homework assignment is a few pragmas for GHC
(these are specific instructions for the compiler). GHC doesn\'t warn about these
behaviors automatically.

> {-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

This next compiler option

> {-# OPTIONS -fdefer-type-errors  #-}

turns type errors into warnings so that you can still run your code in ghci even if
it doesn\'t type check. This flag doesn\'t defer all compile-time errors
(such as errors for unbound variables) and you definitely should fix your type errors.
However, having it in place helps make development more interactive.
Of course, if you try to run the part of your code with a type error in it, it will fail.

When you are finished with the assignment, you should add the option `-Werror`,
which turns all warnings into errors, to the line above. 

Next, we declare that we are creating a module called `Main` and using functions defined
in the modules `Prelude`, `Test.HUnit`, `Data.List` and `Data.Char`.

The `Prelude` line imports all except for the functions listed (which you will write).
The module `Prelude` is special in that it is always imported by default, so the the
point of this line is not to import more functions, but rather to exclude a few funtions.
(Haskell does not allow functions to be redefined in the same module.)

The `Test.HUnit` line imports all functions defined in that module.
The line `Data.List` imports all functions from that module, but makes them
available as `List.intersperse`, etc.

> module Main where
> import Prelude hiding (takeWhile, all, zip, reverse, concat)
> import Test.HUnit
> import qualified Data.List as List
> import qualified Data.Char as Char
> import qualified Data.Maybe as Maybe
> import qualified Text.Read as Read

The `main` program for this assignment runs the tests for each homework problem below.
You should not edit this definition. Instead, your goal is to modify the problems below
so that all of the tests pass. Note that the definitions in Haskell modules do not need
to come in any particular order; here, the main function uses `testStyle`, `testLists`, etc,
even though their definitions come later in the file.

> main :: IO ()
> main = do
>    _ <- runTestTT $ TestList [ testStyle,
>                                testLists,
>                                testWeather,
>                                testSoccer ]
>    return ()

Now that we have the preliminaries out of the way, we can start the actual problems.


Problem (List library chops)
-----------------------------

Define, debug and test the following functions. (Some of these functions are part of
the Haskell standard prelude or standard libraries like `Data.List`. Their solutions
are readily available online. You should not look for these solutions: instead,
implement them yourself.)

For each part of the assignment, you need to declare the type of the function, define it,
and replace the testcase for that part based on the problem description. The declared type
of each function should be the most general one. Make sure to test each of these functions
with multiple inputs using `TestList`. We will be grading your test cases as well as the
correctness and style of your solutions!

> testLists :: Test
> testLists = "testLists" ~: TestList
>   [tintersperse, tinvert, ttranspose, tconcat, tcountSub]
>
> -- The intersperse function takes an element and a list
> -- and intersperses that element between the elements of the list.
> -- For example,
> --    intersperse ',' "abcde" == "a,b,c,d,e"
> --
> -- intersperse is defined in Data.List, and you can test your solution against
> -- that one.
> --
> intersperse = undefined
> tintersperse :: Test
> tintersperse = "intersperse" ~: (assertFailure "testcase for intersperse" :: Assertion)
>
> -- invert lst returns a list with each pair reversed.
> -- for example:
> --   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")]
> --   invert ([] :: [(Int,Char)])  returns []
> --   note, you need to add a type annotation to test invert with []
> --
> invert = undefined
> tinvert :: Test
> tinvert = "invert" ~: (assertFailure "testcase for invert" :: Assertion)
>
> -- concat
> -- The concatenation of all of the elements of a list of lists
> -- for example:
> --    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]
> --
> -- NOTE: remember you cannot use any functions from the Prelude or Data.List for
> -- this problem, even for use as a helper function.
> 
> concat = undefined
> tconcat :: Test
> tconcat = "concat" ~: (assertFailure "testcase for concat" :: Assertion)
>
> -- transpose  (WARNING: this one is tricky!)
> -- The transpose function transposes the rows and columns of its argument.
> -- If the inner lists are not all the same length, then the extra elements
> -- are ignored. Note, this is *not* the same behavior as the library version
> -- of transpose. For example:
> --    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
> --    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]
> --    transpose  [[]] returns []
> -- transpose is defined in Data.List
>
> transpose = undefined
> ttranspose :: Test
> ttranspose = "transpose" ~: (assertFailure "testcase for transpose" :: Assertion)
>
> -- countSub sub str
> -- Return the number of (potentially overlapping) occurrences of substring sub
> -- found in the string str.
> -- for example:
> --      countSub "aa" "aaa" returns 2
>
> countSub = undefined
> tcountSub :: Test
> tcountSub = "countSub" ~: (assertFailure "testcase for countSub" :: Assertion)

Data Munging Kata
---------------------

A Code Kata is an exercise that helps an experienced programmer hone their skills.
The coding exercise is usually not difficult \-\- what is important is the analysis and
design of the problem as well and the practice and repetition that lead to good coding habits.
This exercise comes from website devoted to [Code Kata][codeKata]s and is not specific to Haskell.

[codeKata]: http://codekata.com/kata/kata04-data-munging/ 

Unlike the exercises above, for this problem you are allowed to use functions from
Haskell\'s standard libraries. In particular, you may use list functions from the `Prelude`,
or from `Data.List` in your solution. You may also use functions from `Data.Char`.

This problem is an exercise in three parts to do with real world data. For that reason,
we aren\'t expecting you to produce a robust solution. Your code must work only for the
input test files that we provide. We won\'t test it on any other inputs!
Consequently, your functions must not be partial.

This problem also is about refactoring, so try hard not to read \"ahead\"  \-\- do each of
the three parts below in turn and then reflect on your experience.

**Part One: Hottest Day**

In `jul17.dat` (in the zipfile) you will find daily weather data for Philadelphia,
PA NJ for July 2017. This data is taken from NOAA.

Download this text file, then write a program to output the day number (column one)
with the smallest temperature spread (the maximum temperature is the second column,
the minimum the third column).

We\'ve given you the I/O parts of the program \-\- opening the file and then printing
the final result. Your job is to write the weather function below, that takes the
string containing the text of the file and processes it to find the answer.

> weather :: String -> String
> weather str = error "unimplemented"
> weatherProgram :: IO ()
> weatherProgram = do
>   str <- readFile "jul17.dat"
>   putStrLn (weather str)

The (overloaded) `Read.readMaybe` function can help you convert strings into integers.
We\'ve given it a new name and type signature to make it easier to use.

> readInt :: String -> Maybe Int
> readInt = Read.readMaybe

Here is the test case for this part. If this test fails because it cannot find the
input file, you need to use the :cd command in ghci to make sure that you are in the
right directory.

> testWeather :: Test
> testWeather = "weather" ~: do str <- readFile "jul17.dat"
>                               weather str @?= "6"


**Part Two: Soccer League Table**

The file `soccer.dat` contains the results from the English Premier League for 2001/2.
The columns labeled "F" and "A" contain the total number of goals scored for and against
each team in that season (so Arsenal scored 79 goals against opponents, and had 36 goals
scored against them). Write a program to print the name of the team with the smallest
(absolute) difference in \"for\" and \"against\" goals.

Your program needs to only work with the `soccer.dat` file.
If given a different file it may or may not throw an error.

> soccer :: String -> String
> soccer = error "unimplemented"
> soccerProgram :: IO ()
> soccerProgram = do
>   str <- readFile "soccer.dat"
>   putStrLn (soccer str)
> testSoccer :: Test
> testSoccer = "soccer" ~: do
>   str <- readFile "soccer.dat"
>   soccer str @?= "Aston_Villa"

**Part Three: DRY Fusion**

Now, take the two programs written previously and factor out as much common code as possible,
leaving you with two smaller programs and some kind of shared functionality.

> weather2 :: String -> String
> weather2 = undefined
> soccer2 :: String -> String
> soccer2 = undefined

**Kata Questions**

Think about te following questions:

  * To what extent did the design decisions you made when writing the original
programs make it easier or harder to factor out common code?

  * Was the way you wrote the second program influenced by writing the first?

  * Is factoring out as much common code as possible always a good thing?

  * Did the readability of the programs suffer because of this requirement?

  * How about the maintainability?
