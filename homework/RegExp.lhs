---
title: Regular Expressions
author: Lucilia Figueiredo
---

[regexphs]: RegExp.hs
[regexp-sol]: ../code/RegExp.zip

Download the plain-Haskell version of the file [RegExp][regexphs] and answer each question,
filling in code as appropriate. A solution to this problem will eventually be available [here][regexp-sol]

> {-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs #-}
> {-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

> module RegExp where
> import Prelude hiding(either)
> import Data.Set (Set)
> import qualified Data.Set as Set (singleton, fromList)
> import Data.Map (Map)
> import qualified Data.Map as Map
> import Control.Applicative(Alternative(..))
> import Control.Monad (ap, liftM2)
> import Test.HUnit hiding (State)
> import Test.QuickCheck
> import Test.QuickCheck.Function

> main :: IO ()
> main = return ()

Regular expressions are a specialized language for representing string-matching patterns.
Regular expressions were invented by the mathematician
[Stephen Kleene](https://en.wikipedia.org/wiki/Stephen_Cole_Kleene), one of the pioneers that
created the foundations of the theory of computation (with Goedel, Turing, Church, and Post).
Kleene invented regular expressions to represent the sets of possible behaviors of the abstract
finite computation devices called finite-state machines. In Kleene\'s original formulation, regular
expressions were were built from individual letters with three operators: concatenation, representing
one pattern followed by another; alternation (also called union) denoted by |, representing two
alternative patterns; and closure (also called Kleene-Star), denoted by *, to represent zero or more
repetitions of a pattern. By convention, the empty string represents a special regular expression,
the empty regular expression, which matches the empty string.

For example, the regular expression `a(bc|d)*e` matches all strings that start with `a`, then have
some number of `bc` or `d` characters (possibly none), and end with `e`. Some such strings include `ae`,
`abce`, `abcde`, `adbcde`, `abcbcdbce`. In the 1970s, computer scientists at Bell Labs were working on
the first software tools for text processing, and they adapted and generalized Kleene\'s idea in several
software tools, starting with [grep](https://en.wikipedia.org/wiki/Grep), for searching and editing text.
Regular expressions have many practical uses, mainly in pattern matching, which has applications in
everything from compilers to
searching databases.

In this problem, we consider regular expression evaluators, that is, programs that determine whether
a string is in the language denoted by the regular expression. This process is also called regular expression matching.

We can represent regular expressions using the following datatype in Haskell.

> data RegExp = Char (Set Char)      -- single literal character
>             | Alt RegExp RegExp    -- r1 | r2   (alternation)
>             | Seq RegExp RegExp    -- r1 r2     (concatenation)
>             | Star RegExp          -- r*        (Kleene star)
>             | Empty                -- Îµ, accepts empty string
>             | Void                 -- âˆ…, always fails
>             | Mark String RegExp   -- (for marked subexpressions, see (b) below)
>   deriving (Show, Eq)

Note that the `[Char]` constructor takes a set of characters as argument. It matches any single
character from this set.

While doing the exercise, you may find useful to implement your own instance of the `[Show]` class for
`[RegExp]`s instead of using the one that arising from the deriving annotation above. (In particular,
you might want to print the set of characters associated with a Char in a more compact form than the
one given by the default show instance for `Set`.)

We can define regular expressions for specific character classes:

> char :: Char -> RegExp
> char = Char . Set.singleton

> chars :: String -> RegExp
> chars = Char . Set.fromList

> lower, upper, letter, digit, punc, white, anyc, anyc':: RegExp
> lower  = chars ['a' .. 'z']
> upper  = chars ['A' .. 'Z']
> digit  = chars ['0' .. '9']
> punc   = chars "<>!/.*()?@"
> white  = chars " \n\r\t"

Or the union of all of the above classes:

> anyc'  = lower `Alt` upper `Alt` digit `Alt` punc `Alt` white

A more convenient definition (for debugging purposes, since it will print more compactly) is the following:

> anyc = chars $ ['a' .. 'z']
>                ++ ['A' .. 'Z']
>                ++ ['0' .. '9']
>                ++ "<>!/.*()?@"
>                ++ "\n\r\t"

A regular expression for letters will be also useful:

> letter = chars $ ['A' .. 'Z'] ++ ['a' .. 'z']

Or we can define regular expressions that match specific words.

> word :: String -> RegExp
> word = foldr (Seq . char) Empty

> afp :: RegExp
> afp = word "afp"

The `Star`operator corresponds to 0 or more occurrences of a pattern. For example, this
regular expression accepts any string that begins and ends with the tags <b> and </b>.

> boldHtml :: RegExp
> boldHtml = word "<b>" `Seq` Star anyc `Seq`  word "</b>"

We can use `Star` and `Seq` to define the `plus` operator, which corresponds to one or more
occurrences of a pattern.

> plus :: RegExp -> RegExp
> plus pat = pat `Seq` Star pat

> -- (a)

First, we\'ll write a straightforward (and probably inefficient) operation that determines whether
a particular string is part of the regular language accepted by the given regular expression.
(For now, just ignore 'Mark' constructors in the input regular expressions.)

Begin by implementing the following two helper functions. (You may find that the list monad
is useful for defining and using these functions.)

> -- all decompositions of a string into two different pieces
> --     split "abc" == [("","abc"),("a","bc"),("ab","c"),("abc","")]
> split :: [a] -> [([a], [a])]
> split = error "split: unimplemented"

> -- all decompositions of a string into multi-part (nonempty) pieces
> -- parts "abc" = [["abc"],["a","bc"], ["ab","c"], ["a","b","c"]]
> parts :: [a] -> [[[a]]]
> parts = error "parts: unimplemented"

Now, use split and parts to determine whether the `RegExp` matches the given input string,
your implementation should simply explore all possibilities. For example, to determine whether
the concatenation pattern `Seq r1 r2` matches an input string, use split to compute all possible
ways of splitting the string into two parts and see whether `r1` and `r2` match the two parts.

> accept :: RegExp -> String -> Bool
> 
> accept (Mark _ r)  s = accept r s
> accept _           _ = error "accept: finish me"

> testAccept :: Test
> testAccept = TestList [
>    not (accept Void "a") ~? "nothing is void",
>    not (accept Void "") ~? "really, nothing is void",
>    accept Empty "" ~? "accept Empty true",
>    not (accept Empty "a") ~? "not accept Empty",
>    accept lower "a" ~? "accept lower",
>    not (accept lower "A") ~? "not accept lower",
>    accept boldHtml "<b>cis552</b>!</b>" ~? "cis552!",
>    not (accept boldHtml "<b>cis552</b>!</b") ~? "no trailing" ]

> -- (b)

Backtracking is not the most efficient implementation of regular expressions,
but it is the most easily extensible.

One extension is support for marked subexpressions. For this problem, rewrite `accept`
so that it returns all strings that are matched by the marked subexpressions in the regular expression.

For example, we can mark the part of the regular expression between the tags:

> boldHtmlPat :: RegExp
> boldHtmlPat = word "<b>" `Seq` Mark "<b>" (Star anyc) `Seq` word "</b>"

Or mark sequences of letters that correspond to the first and last names:

> namePat :: RegExp
> namePat = Mark "first" (plus letter) `Seq` Star white `Seq` Mark "last" (plus letter)

Or mark any number of sequences of lowercase letters:

> wordsPat :: RegExp
> wordsPat = Star (Mark "word" (plus lower) `Seq` Star white)

Then, `patAccept` below returns not only whether the pattern matches, but also the parts
of the string that correspond to the marks (in order).

> testPat :: Test
> testPat = TestList [
>     patAccept boldHtmlPat "<b>cis552" ~?= Nothing,
>     patAccept boldHtmlPat "<b>cis552!</b>" ~?=
>         Just (Map.fromList [("<b>",["cis552!"])]),
>     patAccept boldHtmlPat "<b>cis552</b>!</b>" ~?=
>         Just (Map.fromList [("<b>",["cis552</b>!"])]),
>     patAccept namePat "Haskell  Curry" ~?=
>         Just (Map.fromList [("first",["Haskell"]),("last",["Curry"])]),
>     patAccept wordsPat "a    b c   d e" ~?=
>         Just (Map.fromList [("word",["a","b","c","d","e"])])
>   ]

> type Match = Map String [String]

> patAccept :: RegExp -> String -> Maybe Match
> patAccept = error "patAccept: unimplemented"

> -- (c)

You may have noticed by now that this implementation of Regular Expression matching is really slow.
Let\'s fix that.

The textbook way to implement regular expression matching is to first translate the regular
expression into a finite-state machine and then apply the finite-state matching to the string.

However, there\'s a more direct, elegant, but not so well-known alternative, the method of
derivatives due to [Janusz A. Brzozowski](http://maveric.uwaterloo.ca/~brzozo/).
This method is described in more detail
[here](http://blog.sigfpe.com/2005/05/derivatives-of-regular-expressions.html).

The basic idea is that, given a regular expression and the first character in the input
string to match, you can compute a new regular expressions, which must match the remaining
string in order for the original RegExp to match the entire input string. This new regular
expression is called the derivative of the original.

We can use this idea to implement regular expression matching by repeatedly calculating
the derivatives for each character in the string. If the final result is a regular expression
that accepts the empty string, then the original regular expression would have matched the
string. In other words:

> match :: RegExp -> String -> Bool
> match r s = nullable (foldl deriv r s)

Your job is to implement nullable and deriv to complete this implementation.

> -- | `nullable r` return `True` when `r` matches the empty string
> nullable :: RegExp -> Bool
> nullable _ = error "nullable: unimplemented"

> -- |  Takes a regular expression `r` and a character `c`,
> -- and computes a new regular expression that accepts word `w` if `cw` is
> -- accepted by `r`.
> deriv :: RegExp -> Char -> RegExp
> deriv = error "deriv: unimplemented"

For example, if `r` is the literal character `c`, then the derivative of `r` is `Empty`, the
regular expression that only accepts the empty string. In the case of `Seq`, you need to think
about the case where the first regular expression could accept the empty string. In that case,
the derivative should include the possibility that it could be skipped, and the character
consumed by the second regexp.

Note that Haskell\'s lazy evaluation avoids the evaluation of the whole regular expression.
The expression has only to be evaluated as much as nullable needs to calculate an answer.

> -- (d)

We can optimize our match function even further through the use of "smart constructors" for the
regular expression type. These smart constructor recognize simplifications that can be made
when ever a regular expression is put together.

> rStar :: RegExp -> RegExp
> rStar (Star x) = Star x   -- two iterations is the same as one
> rStar Empty    = Empty    -- iterating the empty string is the empty string
> rStar Void     = Empty    -- zero or more occurrences of void is empty
> rStar r        = Star r   -- no optimization

We can quickCheck our optimizations using accept. In each of our three optimization cases,
the rhs and lhs of the definition above should accept the same strings.

We\'ll define this as a property, testing the regexps on strings that contain arbitrary
sequences of a's, b's, c's, and d's.

> (%==%) :: RegExp -> RegExp -> Property
> r1 %==% r2 = forAll (resize 10 (listOf (elements "abcd"))) $
>                  \s -> match r1 s == match r2 s

> prop_rStar :: RegExp -> Property
> prop_rStar r = prop_rStar1 r .&&. prop_rStar2 .&&. prop_rStar3 where
>   prop_rStar1 r = rStar (Star r) %==% Star (Star r)
>   prop_rStar2   = rStar Empty    %==% Star Empty
>   prop_rStar3   = rStar Void     %==% Star Empty

Complete an `Arbitrary` instance for regular expressions to test this property above.
Your regexps should only contain the characters "abcd" and need not contained marked subexpressions.

> instance Arbitrary RegExp where
>    arbitrary = undefined
>    shrink = undefined

Now design and test optimizations for sequencing and alternation:

> rSeq :: RegExp -> RegExp -> RegExp
> rSeq = undefined

> rAlt :: RegExp -> RegExp -> RegExp
> rAlt = undefined

Finally, modify your definition of match above to take advantage of these optimizations.