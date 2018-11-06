---
title: Homework 03: Fold, Datatypes and Trees
date: September 2018
---

[hw03zip]: ../code/hw03.zip
[hw03-sol]: ../code/hw03-sol.zip
[higO]: ../lectures/higherOrder.html
[datatypes]: ../lectures/datatypes.html
[cwebpage]:


Note: this homework is significantly longer than HW02 and covers material from [_Lec3_][higO] and [_Lec4_][datatypes].

> {-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults #-}

Uncomment this line if you found it useful on the previous assignment.

> -- {-# OPTIONS -fdefer-type-errors  #-}

The goal of this homework assignment is practice with fold, user-defined datatypes and trees in Haskell.

This homework is composed of three files: two support files `XMLTypes.hs` and `Play.hs`,
plus the main part of the assignment. For testing, you will also need the file
`sample.html`. All of these files are available [zipped together][hw03zip].

To complete the homework, you should edit only the file `Main.hs`, and submit this
file through the [course web page][cwebpage].

A solution to this homework will eventually be available [here][hw03-sol]

> module Main where
> import Prelude hiding (takeWhile, all, concat)
> import Test.HUnit      -- unit test support
> import XMLTypes        -- support file for XML problem (provided)
> import Play            -- support file for XML problem (provided)

When you load this file into ghci the first time, you may see the following error message:

     /Users/lucilia/afp/hw/hw3/Main.lhs:27:10:
        Could not find module `Play'
        Use -v to see a list of the files searched for.
     Failed, modules loaded: none.

If you do get this error, you\'ll need to change the current directory in ghci so that
it can find the support files (`XMLTypes.hs` and `Play.hs`).
   
You will be able to run the unit tests from ghci after the `XMLTypes` and `Play`
modules have been compiled.

> doTests :: IO ()
> doTests = do
>   _ <- runTestTT $ TestList [ testHO, testFoldr, testTree, testFoldTree, testXML ]
>   return ()
> main :: IO ()
> main = do
>        doTests
>        return ()

Problem - higher-order list operations
-----------------------------------------

Complete these operations which take higher-order functions as arguments.
For extra practice, you may define these operations using `foldr`, but that is not
required for this problem.

> testHO :: Test
> testHO = TestList [ttakeWhile, tfind, tall, tmap2, tmapMaybe]
> -- takeWhile, applied to a predicate p and a list xs,
> -- returns the longest prefix (possibly empty) of xs of elements
> -- that satisfy p:
> -- For example,
> --     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
> --     takeWhile (< 9) [1,2,3] == [1,2,3]
> --     takeWhile (< 0) [1,2,3] == []
>
> takeWhile :: (a -> Bool) -> [a] -> [a]
> takeWhile = undefined
> ttakeWhile :: Test
> ttakeWhile = "takeWhile" ~: (assertFailure "testcase for takeWhile" :: Assertion)
>
> -- find pred lst returns the first element of the list that
> -- satisfies the predicate. Because no element may do so, the
> -- answer is returned in a "Maybe".
> -- for example:
> --     find odd [0,2,3,4] returns Just 3
>
> find :: (a -> Bool) -> [a] -> Maybe a
> find = undefined
> tfind :: Test
> tfind = "find" ~: (assertFailure "testcase for find" :: Assertion)
>
> -- all pred lst returns False if any element of lst
> -- fails to satisfy pred and True otherwise.
> -- for example:
> --    all odd [1,2,3] returns False
>
> all  :: (a -> Bool) -> [a] -> Bool
> all = undefined
> tall :: Test
> tall = "all" ~: (assertFailure "testcase for all" :: Assertion)
>
> -- map2 f xs ys returns the list obtained by applying f to
> -- to each pair of corresponding elements of xs and ys. If
> -- one list is longer than the other, then the extra elements
> -- are ignored.
> -- i.e.
> --   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1]
> --        returns [f x1 y1, f x2 y2, ..., f xn yn]
> --
> -- NOTE: map2 is called zipWith in the Prelude
>
> map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
> map2 = undefined
> tmap2 :: Test
> tmap2 = "map2" ~: (assertFailure "testcase for map2" :: Assertion)
>
> -- mapMaybe
> -- Map a partial function over all the elements of the list
> -- for example:
> --    mapMaybe root [0.0, -1.0, 4.0] == [0.0,2.0]
>
> root :: Double -> Maybe Double
> root d = if d < 0.0 then Nothing else Just $ sqrt d
>
> mapMaybe :: (a -> Maybe b) -> [a] -> [b]
> mapMaybe = undefined
> tmapMaybe :: Test
> tmapMaybe = "mapMaybe" ~: (assertFailure "testcase for mapMaybe" :: Assertion)

Problem - map and foldr practice for lists
-------------------------------------------

Go back to the following functions that you defined in HW03 and redefine them
using a higher-order function such as `map` or `foldr`.

> testFoldr :: Test
> testFoldr = TestList [tinvert, tintersperse, tconcat, tstartsWith, tcountSub]

The `invert` function returns a list with each pair reversed. For example,

    invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")]

> invert :: [(a,b)] -> [(b,a)]
> invert = undefined
> tinvert :: Test
> tinvert = "invert" ~: (assertFailure "testcase for invert" :: Assertion)

The `intersperse` function takes an element and a list and \"intersperses\" that element
between the elements of the list. For example,

    intersperse ',' "abcde" returns "a,b,c,d,e"

> intersperse ::  a -> [a] -> [a]
> intersperse = undefined
> tintersperse :: Test
> tintersperse = "intersperse" ~: (assertFailure "testcase for intersperse" :: Assertion)

The concatenation of all of the elements of a list of lists for example:

    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]

**NOTE:** remember you cannot use any functions from the `Prelude` or `Data.List`
for this problem, even for use as a helper function.

> concat :: [[a]] -> [a]
> concat = undefined
> tconcat :: Test
> tconcat = "concat" ~: (assertFailure "testcase for concat" :: Assertion)

The `startsWith` function returns whether the first argument is a prefix of the second.
For example:

    startsWith "a" "abc"` == True
    startsWith "a" "bac" == False
    startsWith ""  "abc" == True

**NOTE:** use `foldr` for this one, but it is tricky!
(Hint: the value returned by foldr can itself be a function.)

> startsWith :: String -> String -> Bool
> startsWith = undefined
> tstartsWith = "tstartsWith" ~: (assertFailure "testcase for startsWith" :: Assertion)

The `countSub` function returns the number of (potentially overlapping) occurrences of a
substring found in the second argument. For example:

     countSub "aa" "aaa" == 2
     
For this problem, you should first define a variant of `foldr` called `para` and use that
higher-order combinator in your solution. In the case of `cons`, `foldr` provides access
to the head of the list and the result of the fold over the tail of the list. The `para`
function should do the same, but should also provide access to the tail of the list
(before it has been processed). You may also use the `startsWith` function above in `countSub`.

> para :: (a -> [a] -> b -> b) -> b -> [a] -> b
> para = undefined
> countSub  :: String -> String -> Int
> countSub = undefined
> tcountSub = "countSub" ~: (assertFailure "testcase for countSub" :: Assertion)


Problem - Tree processing
---------------------------

The following problems rely on material from [_Lec4_][datatypes].

> testTree :: Test
> testTree = TestList [ tappendTree, tinvertTree, ttakeWhileTree, tallTree, tmap2Tree,
>                       tinfixOrder1, tinfixOrder2 ]

This next problem involves writing some library functions for tree data structures.
The following datatype defines a binary tree, storing data at each internal node.

> -- a basic tree data structure
> data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

This is the definition of a mappping operation for this data structure:

> mapTree :: (a -> b) -> Tree a -> Tree b
> mapTree f Empty = Empty
> mapTree f (Branch x t1 t2) = Branch (f x) (mapTree f t1) (mapTree f t2)

And here is a fold-like operations for trees:

> foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
> foldTree _ e Empty     = e
> foldTree f e (Branch a n1 n2) = f a (foldTree f e n1) (foldTree f e n2)

Use one of these functions to define the following operations over trees.

> -- The appendTree function takes two trees and replaces all of the 'Empty'
> -- constructors in the first with the second tree.  For example:
> --     appendTree (Branch 'a' Empty Empty) (Branch 'b' Empty Empty) returns
> --        Branch 'a' (Branch 'b' Empty Empty) (Branch 'b' Empty Empty)
> appendTree :: Tree a -> Tree a -> Tree a
> appendTree = undefined
>
> tappendTree :: Test
> tappendTree = "appendTree" ~: (assertFailure "testcase for appendTree"  :: Assertion)
>
> -- The invertTree function takes a tree of pairs and returns a new tree
> -- with each pair reversed.  For example:
> --     invertTree (Branch ("a",1) Empty Empty) returns Branch (1,"a") Empty Empty
>
> invertTree :: Tree (a,b) -> Tree (b,a)
> invertTree = undefined
>
> tinvertTree :: Test
> tinvertTree = "invertTree" ~: (assertFailure "testcase for invertTree" :: Assertion)
>
> -- takeWhileTree, applied to a predicate p and a tree t,
> -- returns the largest prefix tree of t  (possibly empty)
> -- where all elements satisfy p.
> -- For example, given the following tree
>
> tree1 :: Tree Int
> tree1 = Branch 1 (Branch 2 Empty Empty) (Branch 3 Empty Empty)
>
> --     takeWhileTree (< 3) tree1  returns Branch 1 (Branch 2 Empty Empty) Empty
> --     takeWhileTree (< 9) tree1  returns tree1
> --     takeWhileTree (< 0) tree1  returns Empty
>
> takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
> takeWhileTree = undefined
>
> ttakeWhileTree :: Test
> ttakeWhileTree = "takeWhileTree" ~: (assertFailure "testcase for takeWhileTree" :: Assertion)
>
> -- allTree pred tree returns False if any element of tree
> -- fails to satisfy pred and True otherwise.
> -- for example:
> --    allTree odd tree1 returns False
>
> allTree :: (a -> Bool) -> Tree a -> Bool
> allTree = undefined
>
> tallTree :: Test
> tallTree = "allTree" ~: (assertFailure "testcase for allTree" :: Assertion)
>
> -- WARNING: This one is a bit tricky!  (Hint: the value
> -- *returned* by foldTree can itself be a function.)
> -- map2Tree f xs ys returns the tree obtained by applying f to
> -- to each pair of corresponding elements of xs and ys. If
> -- one branch is longer than the other, then the extra elements
> -- are ignored.
> -- for example:
> --    map2Tree (+) (Branch 1 Empty (Branch 2 Empty Empty)) (Branch 3 Empty Empty)
> --        should return (Branch 4 Empty Empty)
>
> map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
> map2Tree = undefined
>
> tmap2Tree :: Test
> tmap2Tree = "map2Tree" ~: (assertFailure "testcase for map2Tree" :: Assertion)

Problem - Right and Left tree folds
------------------------------------

> testFoldTree :: Test
> testFoldTree = TestList [ tinfixOrder1, tinfixOrder2, trevOrder, tfoldrTree', tfoldlTree' ]

Consider the `infixOrder` function from the lecture notes.

> infixOrder :: Tree a -> [a]
> infixOrder Empty = []
> infixOrder (Branch x l r) = infixOrder l ++ [x] ++ infixOrder r

For example, using the tree from the lecture

          5
        /   \
       2     9
      / \     \
     1   4     7

> exTree :: Tree Int
> exTree = Branch 5 (Branch 2 (Branch 1 Empty Empty) (Branch 4 Empty Empty))
>                   (Branch 9 Empty (Branch 7 Empty Empty))

produces this behavior.

> testInfixOrder = "infixOrder" ~: infixOrder exTree ~?= [1,2,4,5,9,7]

First, rewrite `infixOrder` using `foldTree`.

> infixOrder1 :: Tree a -> [a]
> infixOrder1 = undefined
>
> tinfixOrder1 = "infixOrder2" ~: infixOrder1 exTree ~?= [1,2,4,5,9,7]

Now consider this foldr variant of `foldTree`. We can think of this function as
first converting the tree to a list with infixOrder and then folding over the list.
However, operationally it is more efficient than that.

> foldrTree :: (a -> b -> b) -> b -> Tree a -> b
> foldrTree _ e Empty = e
> foldrTree f e (Branch k l r) = foldrTree f (f k (foldrTree f e r)) l

Define `infixOrder` in terms of `foldrTree`.

> infixOrder2 :: Tree a -> [a]
> infixOrder2 = undefined
>
> tinfixOrder2 = "infixOrder2" ~: infixOrder2 exTree ~?= [1,2,4,5,9,7]

Now, use `foldrTree` as an inspiration to define a `foldlTree` function,
which folds over the tree in the opposite order.

> foldlTree :: (b -> a -> b) -> b -> Tree a -> b
> foldlTree = undefined
>
> revOrder :: Tree a -> [a]
> revOrder = foldlTree (flip (:)) []
>
> trevOrder = "revOrder" ~: revOrder exTree ~?= [7,9, 5, 4, 2, 1]

Next, define `foldrTree` and `foldlTree` in terms of `foldTree`. (This is challenging!)

> foldrTree' :: (a -> b -> b) -> b ->  Tree a -> b
> foldrTree' = undefined
>
> tfoldrTree' :: Test
> tfoldrTree' = TestList ["foldrTree'" ~: foldrTree' (+) 0 tree1 ~?= 6 ]
>
> foldlTree' :: (b -> a -> b) -> b -> Tree a -> b
> foldlTree' = undefined
>
> tfoldlTree' :: Test
> tfoldlTree' = TestList ["foldlTree'" ~: foldlTree (+) 0 tree1 ~?= 6 ]

Compare your implementations of `infixOrder1` and `infixOrder2`. Which one is preferred? Why?


Problem - XML Transformation
-------------------------------

*WARNING:* this next problem requires some design as well as implementation!

This problem involves transforming XML documents. To keep things simple, we will not
deal with the full generality of XML, or with issues of parsing. Instead, we will
represent XML documents as instances of the following simplified type:

> data SimpleXML =
>      PCDATA  String
>    | Element ElementName [SimpleXML]

> type ElementName = String

A `SimpleXML` value is either a `PCDATA` (\"parsed character data\") node containing
a string, corresponding to a leaf, or else an `Element` node containing a tag and a list of
sub-nodes, corresponding to a branch with arbitrarly many children.

The goal of this exercise is to write a transformation function `formatPlay`, which takes a
play in an XML format specific for plays and converts it to HTML (which is also an XML format).
 
> formatPlay :: SimpleXML -> SimpleXML
> formatPlay = error "implement formatPlay"

The input format is demonstrated by the sample file `Play.hs`.

The XML value in `Play.hs` has the following structure (as it would be written in standard XML syntax):

 <PLAY>
        <TITLE>TITLE OF THE PLAY</TITLE>
        <PERSONAE>
          <PERSONA> PERSON1 </PERSONA>
          <PERSONA> PERSON2 </PERSONA>
          ... -- MORE PERSONAE
       </PERSONAE>
        <ACT>
           <TITLE>TITLE OF FIRST ACT</TITLE>
           <SCENE>
               <TITLE>TITLE OF FIRST SCENE</TITLE>
               <SPEECH>
                   <SPEAKER> PERSON1 </SPEAKER>
                   <LINE>LINE1</LINE>
                   <LINE>LINE2</LINE>
                   ... -- MORE LINES
               </SPEECH>
               ... -- MORE SPEECHES
           </SCENE>
           ... -- MORE SCENES
        </ACT>
        ... -- MORE ACTS
     </PLAY>

The output format is demonstrated by the file `sample.html`. This file contains
a very basic HTML rendition of the same information as `Play.hs`.
You may want to have a look at it in your favorite browser. The HTML in `sample.html`
has the following structure (with whitespace added for readability).
Note that the `<br/>` tags below should be represented as br elements with no children.

  <html>
    <body>
      <h1>TITLE OF THE PLAY</h1>
      <h2>Dramatis Personae</h2>
      PERSON1<br/>
      PERSON2<br/>
      ...
      <h2>TITLE OF THE FIRST ACT</h2>
      <h3>TITLE OF THE FIRST SCENE</h3>
      <b>PERSON1</b><br/>
      LINE1<br/>
      LINE2<br/>
      ...
      <b>PERSON2</b><br/>
      LINE1<br/>
      LINE2<br/>
      ...

      <h3>TITLE OF THE SECOND SCENE</h3>
      <b>PERSON3</b><br/>
      LINE1<br/>
      LINE2<br/>
      ...
    </body>
  </html>
  
Note that your version of `formatPlay` should add no whitespace except what\'s in the
textual data in the original XML.

The test below uses your function to generate a file `dream.html` from the sample play.
Tthe contents of this file after your program runs must be character for character identical
to `sample.html`.

> firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
> firstDiff [] [] = Nothing
> firstDiff (c:cs) (d:ds)
>     | c==d = firstDiff cs ds
>     | otherwise = Just (c:cs, d:ds)
> firstDiff cs ds = Just (cs,ds)
>
> -- | Test the two files character by character, to determine whether
> -- they match.
> testResults :: String -> String -> IO ()
> testResults file1 file2 = do
>   f1 <- readFile file1
>   f2 <- readFile file2
>   case firstDiff f1 f2 of
>     Nothing -> return ()
>     Just (cs,ds) -> assertFailure msg where
>       msg  = "Results differ: '" ++ take 20 cs ++
>             "' vs '" ++ take 20 ds
> testXML :: Test
> testXML = TestCase $ do
>   writeFile "dream.html" (xml2string (formatPlay play))
>   testResults "dream.html" "sample.html"

Important: The purpose of this assignment is not just to "get the job done" i.e.,
to produce the right HTML. A more important goal is to think about what is a good
way to do this job, and jobs like it.

To this end, your solution should be organized into two parts:

  * a collection of generic functions for transforming XML structures that have nothing
  to do with plays, plus

  * a short piece of code (a single function definition or a collection of short functions)
  that uses the generic functions to do the particular job of transforming a play into HTML.

Obviously, there are many ways to do the first part. The main challenge of the assignment
is to find a clean design that matches the needs of the second part. You should worry only for
correctness (producing the required output), but also for the elegance of your solution and
the clarity and readability of your code and documentation. As always, style most definitely counts.

It is strongly recommended that you rewrite this part of the assignment a couple of times:
get something working, then step back and see if there is anything you can abstract out or
generalize, rewrite it, then leave it alone for a few hours or overnight and rewrite it again.
Try to use some of the higher-order programming techniques we\'ve been discussing in class.
