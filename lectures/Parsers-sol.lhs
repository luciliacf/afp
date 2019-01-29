---
title: Parsing with Applicative Functors
date: November 27, 2018
---

[2]: Monads.html

> {-# LANGUAGE InstanceSigs, RankNTypes #-}

> module Parsers where

> import Prelude hiding (filter)
> import Data.Char
> import Text.Read (readMaybe)
> import Data.Maybe (maybeToList,isJust)
> import Control.Applicative


What is a Parser?
-----------------

"A parser for things
Is a function from strings
To lists of pairs
Of things and strings."

-- Graham Hutton

A parser is a piece of software that takes a raw `String` (or sequence
of bytes) and returns some structured object \-\- for example, a list of
options, an XML tree or JSON object, a program's Abstract Syntax Tree,
and so on.  Parsing is one of the most basic computational tasks.  For
example:

- Shell Scripts (command-line options)
- Web Browsers (duh!)
- Games (level descriptors)
- Routers (packets)
- etc.

(Indeed I defy you to find any serious system that does *not* do some
parsing somewhere!)

The simplest way to think of a parser is as a function \-\- i.e\., its
type should be roughly this:

~~~~~{.haskell}
    type Parser = String -> StructuredObject
~~~~~


Composing Parsers
-----------------

The usual way to build a parser is by specifying a grammar and using a
parser generator (e.g., yacc, bison, antlr) to create the actual
parsing function. Despite its advantages, one major limitation of the
grammar-based approach is its lack of modularity. For example, suppose
we have two kinds of primitive values, `Thingy` and `Whatsit`.

       Thingy : ...rule...   { ...action... } ;

       Whatsit : ...rule...  { ...action... } ;

If we want a parser for *sequences of* `Thingy` and `Whatsit` we have
to painstakingly duplicate the rules:

      Thingies : Thingy Thingies  { ... }
                 EmptyThingy      { ... } ;

      Whatsits : Whatsit Whatsits { ... }
                 EmptyWhatsit     { ... } ;

That is, the languages in which parsers are usually described are
lacking in features for modularity and reuse.

In this lecture, we will see how to *compose* mini-parsers for
sub-values to get bigger parsers for complex values.

To do so, we will generalize the above parser type a little bit, by
noting that a (sub-)parser need not (indeed, in general will not)
consume *all* of its input, in which case we need to have the parser
return the unconsumed part of its input:

~~~~~{.haskell}
    type Parser = String -> (StructuredObject, String)
~~~~~

Of course, it would be silly to have different types for parsers for
different kinds of structured objects, so we parameterize the `Parser`
type over the type of structured object that it returns:

~~~~~{.haskell}
    type Parser a = String -> (a, String)
~~~~~

One last generalization is to allow a parser to return a *list* of
possible parse results, where the empty list corresponds to a failure
to parse:

~~~~~{.haskell}
    type Parser a = String -> [(a, String)]
~~~~~

As the last step, let\'s wrap this type definition up as a `newtype` and
define a record component to let us conveniently extract the parser:

> newtype Parser a = P { doParse :: String -> [(a, String)] }

~~~~~~{.haskell}
      ghci> :t doParse
      doParse :: Parser a -> String -> [(a,String)]
~~~~~~

This will make sure that we keep parsers distinct from other values of
this type and, more importantly, will allow us to make parsers an
instance of one or more typeclasses, if this turns out to be
convenient (see below!).

Below, we will define a number of operators on the `Parser` type, which will
allow us to build up descriptions of parsers compositionally.  The actual
parsing happens when we use a parser by applying it to an input string, using
`doParse`.

Now, the parser type might remind you of something else... Remember this?

~~~~~{.haskell}
    newtype State s a = S { runState :: s -> (a, s) }
~~~~~

Indeed, a parser, like a state transformer, [is a monad][2]! There are good
definitions of the `return` and `>>=` functions.

However, most of the time, don\'t need the full monadic structure for parsing.
Just deriving the applicative operators for this type will allow us to parse
any context-free grammar. So in today\'s lecture, keep your eye out for
*applicative* structure for this type.

Now all we have to do is build some parsers!

We\'ll start with some primitive definitions, and then generalize.

Parsing a Single Character
--------------------------

Here\'s a *very* simple character parser that returns the first `Char`
from a (nonempty) string.  Recall the parser type:

~~~~~{.haskell}
    newtype Parser a = P { doParse :: String -> [(a, String)] }
~~~~~

So we need a function that pattern matches its argument, and pulls out
the first character of the string, should it exist.

> get :: Parser Char
> get = P $ \s -> case s of
>                   (c1 : cs) -> [ (c1, cs) ]
>                   []        -> []

Try it out!

~~~~~{.haskell}
    ghci> doParse get "hey!"
    [('h',"ey!")]
    ghci> doParse get ""
    []
~~~~~

See if you can modify the above to produce a parser that looks at the first
char of a (nonempty) string and interprets it as an int. (Hint: remember the
`readMaybe` function.)

> oneDigit :: Parser Int
> oneDigit = P $ \s -> case s of
>                        (c1 : cs) -> case (readMaybe [c1] :: Maybe Int) of
>                                       Just i  -> [ (i, cs) ]
>                                       Nothing -> []
>                        [] -> []
> 

~~~~~{.haskell}
    ghci> doParse oneDigit "1"
    [(1,"")]
    ghci> doParse oneDigit "12"
    [(1,"2")]
    ghci> doParse oneDigit "hey!"
    []
~~~~~

And here\'s a parser that looks at the first char of a string and interprets it
as the unary negation operator, if it is '-', and an identity function if it
is '+'.

> oneOp :: Parser (Int -> Int)
> oneOp = P $ \s -> case s of
>                    ('-' : cs) -> [ (negate, cs) ]
>                    ('+' : cs) -> [ (id, cs) ]
>                    _          -> []

~~~~~{.haskell}
    ghci> fst (head (doParse oneOp "-")) 3
    -3
    ghci> fst (head (doParse oneOp "+")) 3
    3
~~~~~


Can we generalize this pattern? What if we pass in a function that specifies whether
the character is of interest.  The `satisfy` function constructs a parser that succeeds
if the first character satisfies the predicate.

> satisfy :: (Char -> Bool) -> Parser Char
> 
> satisfy f = P $ \s -> case s of
>                    (c : cs) -> if f c then [(c,cs)] else []
>                    []        -> []
> 

~~~~~{.haskell}
    ghci> doParse (satisfy isAlpha) "a"
    [('a',"")]
    ghci> doParse (satisfy isUpper) "a"
    []
~~~~~

     SPOILER SPACE
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

Here\'s how I implemented `satisfy`, taking advantage of the list monad.

> satisfy' :: (Char -> Bool) -> Parser Char
> satisfy' f = P $ \s -> [ (c,cs) | (c,cs) <- doParse get s, f c ]

With this implementation, we can see that we can generalize again, so that it
works for any parser, not just get...

> filter :: (a -> Bool) -> Parser a -> Parser a
> filter f p = P $ \s -> [ (c,cs) | (c, cs) <- doParse p s, f c ]


Parser is a Functor
===================

The name `filter` is directly inspired by the filter function for lists. And
indeed, just like we can think of `[a]` as a way to get values of type a, we
can likewise think of `Parser a` as a way to potentially get a value of type `a`.

So, are there other list-like operations that our parsers should support?

Of course!


> instance Functor Parser where
>     fmap :: (a -> b) -> Parser a -> Parser b
>     
>     fmap f p = P $ \s -> do (c, cs) <- doParse p s
>                             return (f c, cs)
>     


With (`get`, `satisfy`, `filter` and `fmap`) we now have a small library of
that we can use to build new (single character) parsers.

For example, we can write some simple parsers for particular sorts of
characters.  The following definitions parse alphabetic and numeric characters
respectivel.

> alphaChar, digitChar :: Parser Char
> alphaChar = satisfy isAlpha
> digitChar = satisfy isDigit

~~~~~~~~~~~~~~~~~~{.haskell}
    ghci> doParse alphaChar "123"
    []
    ghci> doParse digitChar "123"
    [('1',"23")]
~~~~~~~~~~~~~~~~~~~

And now we can use `fmap` to rewrite oneDigit

> oneDigit' :: Parser Int
> oneDigit' = cvt <$> digitChar where    -- fmap!
>   cvt :: Char -> Int
>   cvt c = ord c - ord '0'


~~~~~{.haskell}
    ghci> doParse oneDigit' "92"
    [(9,"2")]
    ghci> doParse oneDigit' "cat"
    []
~~~~~

Finally, finish this parser that should parse just one specific `Char`:

> char :: Char -> Parser Char
> char c = satisfy (== c)


~~~~~~~~~~~{.haskell}
    ghci> doParse (char 'a') "ab"
    [('a',"b")]
    ghci> doParse (char 'a') "ba"
    []
~~~~~~~~~~~~~~~~~~~~~



Parser Composition
==================

What if we want to parse more than one character from the input?

Using `get` we can write a composite parser that returns a pair of
the first two `Char` values from the front of the input string:

> twoChar0 :: Parser (Char, Char)
> twoChar0 = P $ \s -> do (c1, cs)  <- doParse get s
>                         (c2, cs') <- doParse get cs
>                         return ((c1,c2), cs')

~~~~~~~~~~~{.haskell}
     ghci> doParse twoChar0 "ab"
     [(('a','b'),"")]
~~~~~~~~~~~~~~~~~~~~~


More generally, we can write a *parser combinator* that takes two
parsers and returns a new parser that uses first one and then the
other and returns the pair of resulting values...

> pairP0 ::  Parser a -> Parser b -> Parser (a,b)
> 
> pairP0 p1 p2 =  P $ \s -> do (c1, cs)  <- doParse p1 s
>                              (c2, cs') <- doParse p2 cs
>                              return ((c1,c2), cs')
> 

and use that to rewrite 'twoChar' more elegantly like this:

> twoChar1 :: Parser (Char, Char)
> twoChar1 = pairP0 get get

~~~~~{.haskell}
    ghci> doParse twoChar1 "hey!"
    [(('h','e'),"y!")]
    ghci> doParse twoChar1 ""
    []
~~~~~

Parser is an Applicative Functor
================================

Suppose we want to parse *two* characters, where the first should be a sign
and the second a digit?

We\'ve already defined single character parsers that should help. We just need
to put them together.

        oneOp    :: Parser (Int -> Int)
        oneDigit :: Parser Int

And we put them together in a way that looks a bit like `fmap` above. However,
instead of passing in the function as a parameter, we get it via parsing.

> signedDigit0 :: Parser Int
> signedDigit0 = P $ \ s -> do (f, cs)  <- doParse oneOp s
>                              (x, cs') <- doParse oneDigit cs
>                              return (f x, cs')

~~~~~{.haskell}
     ghci> doParse signedDigit0 "-1"
     ghci> doParse signedDigit0 "+3"
~~~~~

Can we generalize this pattern? What is the type when `oneOp` and `oneDigit`
are arguments to the combinator?

> apP p1 p2 = P $ \ s -> do (f, s') <- doParse p1 s
>                           (x,s'') <- doParse p2 s'
>                           return (f x, s'')

Does this type look familiar?

~~~~~{.haskell}
     ghci> :t apP
     apP :: Parser (t -> a) -> Parser t -> Parser a
~~~~~

Whoa! That is the type of the `(<*>)` operator from the `Applicative` class.
What does this combinator do?  It just grabs all function values out of the
first parser and then grabs all of the arguments (using the remaining part of
the string) from the second parser, and then return all of the applications.

What about pure?

The definition of `pure` is very simple \-\- we can let the types
guide us. This parser produces a specific character without consuming
any of the input string.

> pureP :: a -> Parser a
> pureP x = P $ \s -> [(x,s)]

So we can put these two definitions together in our class instance.

> instance Applicative Parser where
>   pure   = pureP
>   (<*>)  = apP

Let\'s go back and reimplement our examples with the applicative combinators:

> twoChar :: Parser (Char, Char)
> twoChar = pure (,) <*> get <*> get where
>    -- (,) is the same as \x -> \y -> (x,y)

> signedDigit :: Parser Int
> signedDigit = oneOp <*> oneDigit

~~~~~{.haskell}
    ghci> doParse twoChar "hey!"
    ghci> doParse twoChar ""
    ghci> doParse signedDigit "-1"
    ghci> doParse signedDigit "+3"
~~~~~~

Now we\'re picking up speed.  First, we can use our combinators to rewrite
our more general pairing parser (`pairP`) like this:

> pairP :: Parser a -> Parser b -> Parser (a,b)
> pairP p1 p2 = pure (,) <*> p1 <*> p2

We can even dip into the `Control.Applicative` library and write `pairP` even
more succinctly:

-- liftA2 f p1 p2 = pure f <*> p1 <*> p2

> pairP' :: Parser a -> Parser b -> Parser (a,b)
> pairP' = liftA2 (,)

And, `Control.Applicative` gives use even more options for constructing
parsers.

> tripleP :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
> tripleP = liftA3 (,,)

For example, the `*>` and `<*` operators are also defined in
`Control.Applicative`. Take a look at their definitions and see if you can
figure out what they do. `(*>) :: f a -> f b -> f b` sequences actions,
discarding the value of the first argument.
`(<*) :: f a -> f b -> f a` sequences actions, discarding the value of the
second argument.

> parenP :: Char -> Parser b -> Char -> Parser b
> parenP open p close = char open *> p <* char close

~~~~~{.haskell}
     ghci> doParse (parenP '(' get ')') "(1)"
     [('1',"")]
~~~~~


Recursive Parsing
-----------------

However, to parse more interesting things, we need to add some kind of
recursion to our combinators. For example, it\'s all very well to parse
individual characters (as in `char` above), but it would a lot more fun if we
could recognize whole `String`s.

Let\'s try to write it!

> string :: String -> Parser String
> string ""     = pure ""
> string (x:xs) = liftA2 (:) (char x) (string xs)


Much better!

~~~~~{.haskell}
    ghci> doParse (string "mic") "mickeyMouse"
    ghci> doParse (string "mic") "donald duck"
~~~~~

For fun, try to write `string` using `foldr` for the list recursion.

> string' :: String -> Parser String
> string' = foldr (\x y -> liftA2 (:) (char x) y) (pure "")
> 

Furthermore, we can use natural number recursion to write a parser that grabs
`n` characters from the input:

> grabn :: Int -> Parser String
> grabn n = if n <= 0 then pure "" else liftA2 (:) get (grabn (n-1))


~~~~~{.haskell}
    ghci> doParse (grabn 3) "mickeyMouse"
    [("mic","keyMouse")]
    ghci> doParse (grabn 3) "mi"
    []
~~~~~


Nondeterministic Choice
=======================

The `Applicative` operators give us sequential composition of parsers
(i.e. run one parser then another). But what about parallel composition?

Let\'s write a combinator that takes two sub-parsers and nondeterministically
chooses between them.

> chooseP :: Parser a -> Parser a -> Parser a

How to write it?  Well, we want to return a succesful parse if
*either* parser succeeds. Since our parsers return multiple values, we
can simply return the *union* of all the results!

> p1 `chooseP` p2 = P $ \s ->  let
>                                res1 = doParse p1 s
>                                res2 = doParse p2 s
>                              in res1 ++ res2

We can use the above combinator to build a parser that
returns either an alphabetic or a numeric character

> alphaNumChar = alphaChar `chooseP` digitChar

~~~~~{.haskell}
    ghci> doParse alphaNumChar "cat"
    [('c',"at")]
    ghci> doParse alphaNumChar "2cat"
    [('2',"cat")]
~~~~~

With `chooseP`, if *both* parsers succeed, then we get back all the results.
For example, with this parser

> grab2or4 = grabn 2 `chooseP` grabn 4

we will get back *two* results if both parses are possible,

~~~~~{.haskell}
    ghci> doParse grab2or4 "mickeymouse"
    [("mi","ckeymouse"),("mick","eymouse")]
~~~~~

and only one if the other is not possible:

~~~~~{.haskell}
    ghci> doParse grab2or4 "mic"
    [("mi","c")]
    ghci> doParse grab2or4 "m"
    []
~~~~~

Even with just these rudimentary parsers that we have at our disposal, we can
start doing some interesting things. For example, here is a little
calculator. First, we parse arithmetic operations as follows:

> intOp = plus `chooseP` minus `chooseP` times `chooseP` divide
>   where plus   = char '+' *> pure (+)
>         minus  = char '-' *> pure (-)
>         times  = char '*' *> pure (*)
>         divide = char '/' *> pure div

(Can you guess the type of the above parser? Ask ghci if you are unsure.)
Then we parse simple expressions by parsing a digit or parsing a digit
followed by an operator and another calculation.

> infixAp :: Applicative f => f a -> f (a -> b -> c) -> f b -> f c
> infixAp = liftA3 (\i1 o i2 -> o i1 i2)

> calc :: Parser Int
> calc = oneDigit `chooseP` infixAp oneDigit intOp calc

This parser, when run, will perform both parsing and calculation. It will also
produce all of the intermediate results.

~~~~~{.haskell}
    ghci> doParse calc "8/2"
    [(8,"/2"),(4,"")]
    ghci> doParse calc "8+2+3"
    [(8,"+2+3"),(10,"+3"),(13,"")]
~~~~~


Parsing With a List of Sub-Parsers
----------------------------------

Let\'s write a combinator that takes a parser `p` that returns an `a`
and constructs a parser that recognizes a sequence of strings (each
recognized by `p`) and returns a *list* of `a` values. That is, it
keeps grabbing `a` values as long as it can and returns them as `[a]`.

We can do this by writing a parser that either parses one thing (if possible)
and then calls itself recursively or succeeds without consuming any
input. Fill in the first part of the `chooseP` below.

> manyP :: Parser a -> Parser [a]
> 
> manyP p = liftA2 (:) p (manyP p) `chooseP` pure []
> 

Beware: This parser can yield a lot of results!

~~~~~{.haskell}
    ghci> doParse (manyP oneDigit) "12345a"
    [([1,2,3,4,5],"a"),([1,2,3,4],"5a"),([1,2,3],"45a"),([1,2],"345a"),([1],"2345a"),([],"12345a")]
~~~~~

What happens if we swap the order of the `chooseP`?

Deterministic Choice
--------------------

Often, what we want is a single result, not a long list of potential results.
To get this, we want a *deterministic* choice combinator. This combinator
returns no more than one result \-\- i.e\., it runs the choice parser but
discards extra results.

> chooseFirstP :: Parser a -> Parser a -> Parser a
> chooseFirstP p1 p2 = P $ \s -> take 1 (doParse (p1 `chooseP` p2) s)

Note that this parser is *extremely sensitive* to the order of the
arguments. If `p1` produces output, we will never try to parse with `p2`.

We can use deterministic choice and failure together to make the `Parser` type an instance
of the `Alternative` type class from
[Control.Applicative](https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Applicative.html).

> instance Alternative Parser where
>    empty = failP
>    (<|>) = chooseFirstP

The `Alternative` class also requires us to have a *failure* parser that never
parses anything (i.e. one that always returns `[]`):

> failP :: Parser a
> failP = P $ \ s -> []


This instance automatically gives us definitions of the functions `many` and
`some`.  Their default definitions look something like this:

~~~~~~~~~~{.haskell}
    many :: Alternative f => f a -> f [a]
    many v = some v <|> pure []

    some :: Alternative f => f a -> f [a]
    some v = liftA2 (:) v (many v)
~~~~~~~~~~

The `many` combinator returns a single, maximal sequence produced by iterating
the given parser.

~~~~~{.haskell}
    ghci> doParse (many digitChar) "12345a"
~~~~~

Let\'s use `some` to write a parser that will return an entire
natural number (not just a single digit.)

> oneNat :: Parser Int
> oneNat = fmap read (some digitChar)   -- read will succeed because input is all digits

~~~~~{.haskell}
    ghci> doParse oneNat "12345a"
    ghci> doParse oneNat ""
~~~~~

Now use the `Alternative` operators to implement a parser that parses zero or
more occurrences of `p`, separated by `sep`.

> sepBy :: Parser a -> Parser b -> Parser [a]
> 
> -- correct version
> sepBy p sep = liftA2 (:) p (many (sep *> p)) <|> pure []
> -- doesn't consume any input with trailing separator
> sepBy2 p sep = (liftA2 append (many (p <* sep )) p) <|> pure [] where
>    append l e = l ++ [e]
> -- doesn't accept the empty string
> sepBy3 p sep = ((:) <$> p <*> (many (sep *> p)))
> -- doesn't correctly parse "111" when the separator is "1"
> sepBy4 p sep = many (p <|> (sep *> p))

> go :: (forall a b. Parser a -> Parser b -> Parser [a]) -> IO ()
> go sepBy = do
>    print $  doParse (sepBy oneNat (char ',')) "1"
>    print $  doParse (sepBy oneNat (char ',')) "1,12"
>    print $  doParse (sepBy oneNat (char ',')) "1,12,0,"
>    print $  doParse (sepBy oneNat (char ',')) "1,12,,0"
>    print $  doParse (sepBy (char '1') (char '1')) "111"
>    print $  doParse (sepBy (char '1') (char '1')) "11"
>    print $  doParse (sepBy oneNat (char ',')) ""
>    print $  doParse (sepBy oneNat (char ',')) ","

~~~~~{.haskell}
     ghci > doParse (sepBy oneNat (char ',')) "1,12,0,3"
     [([1,12,0,3],"")]
     ghci> doParse (sepBy oneNat (char ',')) "1"
     [([1],"")]
     ghci > doParse (sepBy oneNat (char ',')) "1,12,0,"
     ghci > doParse (sepBy oneNat (char '8')) "888"
     ghci > doParse (sepBy oneNat (char ',')) ""


~~~~~

Parsing Arithmetic Expressions
==============================

Now let\'s use the above to build a small calculator, that parses and
evaluates arithmetic expressions. In essence, an expression is either
a binary operand applied to two sub-expressions or else an integer. We
can state this as:


> calc1 ::  Parser Int
> calc1 =  infixAp oneNat intOp calc1 <|> oneNat

This works pretty well...

~~~~~{.haskell}
    ghci> doParse calc1 "1+2+33"
    ghci> doParse calc1 "11+22-33"
~~~~~

But things get a bit strange with minus:

~~~~~{.haskell}
    ghci> doParse calc1 "11+22-33+45"
~~~~~

Huh?  Well, if you look back at the code, you\'ll realize the
above was parsed as

~~~~~{.haskell}
    11 + ( 22 - (33 + 45))
~~~~~

because in each `binExp` we require the left operand to be an
integer. In other words, we are assuming that each operator is *right
associative* hence the above result.  Making this parser left
associative is harder than it looks \-\- we can\'t just swap 'oneNat' and
'calc1' above.  (Why not?)

Furthermore, things also get a bit strange with multiplication:

~~~~~{.haskell}
    ghci> doParse calc1 "10*2+100"
~~~~~

This string is parsed as:

~~~~~{.haskell}
    10 * (2 + 100)
~~~~~

But the rules of precedence state that multiplication should bind
tighter that addition. So even if we solve the associativity problem
we\'ll still need to be careful.

Precedence
----------

We can add both left associativity and precedence by stratifying the
parser into different levels.  Here, let\'s split our binary operations
into addition-like and multiplication-like ones.

> addOp :: Parser (Int -> Int -> Int)
> addOp = char '+' *> pure (+) <|> char '-' *> pure (-)

> mulOp :: Parser (Int -> Int -> Int)
> mulOp = char '*' *> pure (*) <|> char '/' *> pure div

Now, we can stratify our language into mutually recursive sub-languages, where
each top-level expression is parsed as a *sum-of-products*. Product
expressions are then composed of factors: either natural numbers or arbitrary
expressions inside parentheses.

> calc2 :: Parser Int
> calc2 = sumE

> sumE :: Parser Int
> sumE = infixAp prodE addOp sumE <|> prodE

> prodE :: Parser Int
> prodE = infixAp factorE mulOp prodE <|> factorE

> factorE :: Parser Int
> factorE = oneNat <|> parenP '(' calc2 ')'

~~~~~{.haskell}
    ghci> doParse calc2 "10*2+100"
    ghci> doParse calc2 "10*(2+100)"
~~~~~

Do you understand why the first parse returned `120` ?  What would
happen if we *swapped* the order of the alternatives?


Parsing Pattern: Chaining
-------------------------

There is not much point gloating about combinators if we are going to
write code like the above \-\- the bodies of `sumE` and `prodE` are
almost identical!

Let\'s take a closer look at them. In essence, a `sumE` is
of the form:

~~~~~{.haskell}
    prodE + ( prodE + ( prodE + ... prodE ))
~~~~~

That is, we keep chaining together `prodE` values and adding them for
as long as we can. Similarly a `prodE` is of the form

~~~~~{.haskell}
    factorE * ( factorE * ( factorE * ... factorE ))
~~~~~

where we keep chaining `factorE` values and multiplying them for as
long as we can.

But we\'re still not done: we need to fix the associativity problem:

~~~~~{.haskell}
    ghci> doParse sumE "10-1-1"
~~~~~

Ugh! I hope you understand why: it's because the above was parsed as
`10 - (1 - 1)` (right associative) and not `(10 - 1) - 1` (left
associative). You might be tempted to fix that simply by flipping the order
in `infixAp`, thus

~~~~~{.haskell}
    sumE = addE <|> prodE
      where addE = liftA3 (\x o y -> x `o` y) prodE addOp sumE
~~~~~

but this would be disastrous. Can you see why?  The parser for `sumE`
directly (recursively) calls itself *without consuming any input!*
Thus, it goes off the deep end and never comes back.

Instead, we want to parse the input as a `prod` expression followed by any
number of addition operators and other product expressions. We can temporarily
store the operators and expressions in a list of pairs.  Then, we\'ll `foldl`
over this list, using the each operator to combine the current result with the
next number.


> sumE1 :: Parser Int
> sumE1 = foldl comb <$> first <*> rest where

>            comb :: Int -> (Int -> Int -> Int, Int) -> Int
>            comb = \ x (op,y) -> x `op` y

>            first :: Parser Int
>            first = prodE1

>            rest :: Parser [(Int -> Int -> Int, Int)]
>            rest = many ((,) <$> addOp <*> prodE1)

> prodE1 :: Parser Int
> prodE1 = foldl comb <$> factorE1 <*> rest where
>            comb = \ x (op,y) -> x `op` y
>            rest = many ((,) <$> mulOp <*> factorE1)

> factorE1 :: Parser Int
> factorE1 = oneNat <|> parenP '(' sumE ')'

The above is indeed left associative:

~~~~~{.haskell}
    ghci> doParse sumE1 "10-1-1"
~~~~~

Also, it is very easy to spot and bottle the chaining computation
pattern: the only differences are the *base* parser (`prodE1` vs
`factorE1`) and the binary operation (`addOp` vs `mulOp`).  We simply
make those parameters to our *chain-left* combinator:

> chainl1 :: Parser b -> Parser (b -> b -> b) -> Parser b
> p `chainl1` pop = foldl comb <$> p <*> rest where
>            comb = \ x (op,y) -> x `op` y
>            rest = many ((,) <$> pop <*> p)

after which we can rewrite the grammar in three lines:

> sumE2    = prodE2   `chainl1` addOp
> prodE2   = factorE2 `chainl1` mulOp
> factorE2 = parenP '(' sumE2 ')' <|> oneNat

~~~~~{.haskell}
    ghci> doParse sumE2 "10-1-1"
    ghci> doParse sumE2 "10*2+1"
    ghci> doParse sumE2 "10+2*1"
~~~~~


This concludes our exploration of applicative parsing, but what we\'ve covered
is really just the tip of an iceberg. Though parsing is a very old problem,
studied since the dawn of computing, algebraic structures in Haskell bring a
fresh perspective that has now been transferred from Haskell to many other
languages.