---
title: Homework 09: State Monad and Applicative Parsing
---

[parsinghs]: ../code/Parsing.zip
[testimp]: [test.imp]
[factimp]: [fact.imp]
[absimp]: [abs.imp]
[timesimp]: [times.imp]


This homework provides practice with the state monad and applicative parsing
combinators that we developed in class by developing and implementing a small,
imperative programming language.

> {-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
> module Main where

Before starting this assignment:

Download a [zip file][parsingzip] containing all of the source files that you need
for this assignment.

You will need to edit `Main.hs` and `Tests.hs`, the plain-Haskell version of the
assignment files.

Remember that, as you complete the assignment, you should be testing.
All of the test cases and properties are in `Tests.hs`. You should edit this
file to add your own testing code. The test cases will also help specify parts
of the problems.

Read over the `.imp` files for this assignment, which are sample files for a
simple imperative programming language (the WHILE language). Take a look at
these files to get an idea of the concrete syntax of the WHILE language.

      [test.imp][testimp], [fact.imp][factimp], [abs.imp][absimp], [times.imp][timesimp].

Note, although the concrete syntax looks like C or Java, it is not exactly the same
as those languages.

This homework assignment draws on many libraries. So that you can tell where to look for the
definitions, we will either import them explicitly (as in `Applicative`/`Monad` below) or qualify
the imports (as in `Data.Map` and `Text.PrettyPrint`). If you want to use more functions from
`Applicative`/`Monad`, you may add them to the explicit import list.

> import Data.Map (Map)
> import qualified Data.Map as Map
> import Text.PrettyPrint (Doc)
> import qualified Text.PrettyPrint as PP
> import Control.Applicative (Alternative(..))
> import Control.Monad ()

This assignment uses the State Monad library that we developed in the lecture notes.
The relevant definitions are in the file `State.hs`. Operations such as get and put are
imported as `S.get` and `S.put`.

> import State (State)
> import qualified State as S

This assignment also uses the parsing library from the lecture notes. The relevant definitions
are in the files `Parser.hs` and `ParserCombinators.hs`.

> import qualified Parser as P
> import qualified ParserCombinators as P

> main :: IO ()
> main = return ()


An Interpreter for WHILE
=========================

In this problem, you will use monads to build an evaluator for the simple imperative language.
In this language, we will represent program variables as strings:

> type Variable = String

Programs in the language are simply sequences of statements in a block

> newtype Block =
>     Block [ Statement ]                 -- { s1; ... sn; }
>   deriving (Eq, Show)

and statements themselves can be one of three flavors

> data Statement =
>     Assign Variable Expression          -- x = e
>   | If Expression Block Block           -- if e then s1 else s2
>   | While Expression Block              -- while e do s
>   deriving (Eq, Show)

and expressions are variables, constants or binary operators applied to sub-expressions

> data Expression =
>     Var Variable                        -- x
>   | Val Value                           -- v
>   | Op Expression Bop Expression
>   deriving (Eq, Show)

and binary operators are simply two-argument functions:

> data Bop =
>     Plus     -- +  :: Int -> Int -> Int
>   | Minus    -- -  :: Int -> Int -> Int
>   | Times    -- *  :: Int -> Int -> Int
>   | Divide   -- /  :: Int -> Int -> Int
>   | Gt       -- >  :: Int -> Int -> Bool
>   | Ge       -- >= :: Int -> Int -> Bool
>   | Lt       -- <  :: Int -> Int -> Bool
>   | Le       -- <= :: Int -> Int -> Bool
>   deriving (Eq, Show, Enum)

and Constant values are either integers or booleans

> data Value =
>     IntVal Int
>   | BoolVal Bool
>   deriving (Eq, Show)

We will represent the store i.e\. the machine\'s memory, as an associative
map from `Variable` to `Value`:

> type Store = Map Variable Value

*Note*: we don\'t have exceptions (yet!), so if a variable is not found (eg because
it is not initialized) simply return the value 0. In the future, we will add this as
a case where exceptions are thrown (the other case being type errors.)

Test Programs
---------------

> -- Here are some test programs. You can ignore the 80-column limit for this part
> -- of the file.
> -- test.imp
> wTest :: Block
> wTest = Block [Assign "x" (Op (Op (Op (Val (IntVal 1)) Plus (Val (IntVal 2))) Minus (Val (IntVal 3))) Plus (Op (Val (IntVal 1)) Plus (Val (IntVal 3)))),
>               Assign "y" (Val (IntVal 0)),
>               While (Op (Var "x") Gt (Val (IntVal 0)))
>                 (Block [Assign "y" (Op (Var "y") Plus (Var "x")),
>                         Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))])]

> -- fact.imp
> wFact :: Block
> wFact =  Block [ Assign "n" (Val (IntVal 5)),
>                 Assign "f" (Val (IntVal 1)),
>                 While (Op (Var "n") Gt (Val (IntVal 0)))
>                  (Block [Assign "x" (Var "n"),
>                          Assign "z" (Var "f"),
>                          While (Op (Var "x") Gt (Val (IntVal 1))) (Block [Assign "f" (Op (Var "z") Plus (Var "f")),
>                                                                           Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))]),
>                          Assign "n" (Op (Var "n") Minus (Val (IntVal 1)))]) ]

> -- abs.imp
> wAbs :: Block
> wAbs = Block [Assign "x" (Op (Val (IntVal 0)) Minus (Val (IntVal 3))),
>              If (Op (Var "x") Lt (Val (IntVal 0)))
>                 (Block [Assign "x" (Op (Val (IntVal 0)) Minus (Var "x"))]) (Block [])]

> -- times.imp
> wTimes :: Block
> wTimes = Block [Assign "x" (Val (IntVal 10)),
>                 Assign "y" (Val (IntVal 3)),
>                 Assign "z" (Val (IntVal 0)),
>                 While (Op (Var "x") Gt (Val (IntVal 0))) (Block [Assign "z" (Op (Var "z") Plus (Var "y")),
>                                                                  Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))])]


Expression Evaluator
---------------------

First, write a function

> evalE :: Expression -> State Store Value

that takes as input an expression and returns a state-transformer that yields a `Value`.
(Yes, right now, the transformer doesn\'t really transform the world, but we will use the
monad structure later.)

Again, we don\'t have any exceptions or typechecking, so the interpretation of any
invalid binary operations (such as '2 + True') should just be 0.

Your expression evaluator should be total. For any input it should produce some value.

> evalE (Var _)    = error "evalE: unimplemented"
> evalE (Val _)    = error "evalE: unimplemented"
> evalE (Op _ _ _) = error "evalE: unimplemented"


Statement Evaluator
---------------------

Next, write a function

> evalS :: Statement -> State Store ()

that takes as input a statement and returns a world-transformer that returns a unit.
Here, the world-transformer should in fact update the input store appropriately with the
assignments executed in the course of evaluating the Statement.

> evalS (While _ _)      = error "evalS: unimplemented"
> evalS (Assign _ _)     = error "evalS: unimplemented"
> evalS (If _ _ _)       = error "evalS: unimplemented"

In the If and While cases, if e evaluates to a non-boolean value, just skip the rest of the statement.

Now, write a function for statement blocks that evaluates each one in sequence.

> eval :: Block -> State Store ()
> eval (Block _)        = error "eval: unimplemented"

Finally, write a function

> exec :: Block -> Store -> Store
> exec = error "exec: unimplemented"

such that `execS stmt` store returns the new `Store` that results from evaluating the command
block from the world store. Hint: You may want to use the following library function from the
`State` module.

~~~~~{.Haskell}
      execState :: State s a -> s -> s
~~~~~~~

When you are done with the above, the following function will \"run\" a block of statements
starting with the empty store (where no variable is initialized). Running the program should
print the value of all variables at the end of execution.

> run :: Block -> IO ()
> run block = do putStrLn "Output Store:"
>                print (exec block Map.empty)

Your interpreter should pass the tests in Tests.hs for evaluating the sample test files.
Make sure to add your own tests.

A Pretty Printer for WHIL
==========================

The derived `Show` instances for the datatypes above are pretty hard to read, especially when
programs get long. Really, who wants to read this...

      Block [ Assign "n" (Val (IntVal 5)), Assign "f" (Val (IntVal 1)), While (Op (Var "n") Gt (Val (IntVal 0))) (Block [Assign "x" (Var "n"), Assign "z" (Var "f"), While (Op (Var "x") Gt (Val (IntVal 1))) (Block [Assign "f" (Op (Var "z") Plus (Var "f")), Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))]), Assign "n" (Op (Var "n") Minus (Val (IntVal 1)))]) ]

instead of this\.\.\.

     n = 5; f = 1; while (n > 0) { x = n; z = f; while (x > 1) { f = z + f; x = x - 1; }; n = n - 1; }

or even this?

   n = 5;
   f = 1;
   while (n > 0) {
     x = n;
     z = f;
     while (x > 1) {
       f = z + f;
       x = x - 1;
     }
     n = n - 1;
   }

A *pretty printer* is a function that converts an abstract syntax tree into a readable representation
of the concrete syntax. Your job on this problem is to use the
[HughesPJ](http://hackage.haskell.org/package/pretty)
library to develop a pretty printer for WHILE.

For background reading, [Ch. 5](http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html)
of Read World Haskell goes through the design of a library, called \"Prettify\", which is a simplified
version of the HughesPJ library. Note that most of the definitions
in the library (such as char, text, etc.) are imported qualified as PP.char so that they do not
conflict with the other parts of this assignment.

The HughesPJ library provides the following to assist in the development of pretty printers:

  * An abstract type Doc of \"pretty documents\" that know how to lay themselves out prettily.
    We can use this type to define a class of of types that support pretty printing \-\- those that
    define a function mapping any value of that type to a suitable Doc.
    
> class PP a where
>   pp :: a -> Doc

  * Primitive documents and operations for constructing Docs from primitive types, such as
    characters and string.

  -- A document of height and width 1, containing a literal character.
  char :: Char -> Doc

  -- A document of height 1 containing a literal string.
  text :: String -> Doc

  -- An empty document (renders as the empty string)
  empty :: Doc

  -- a ';' character
  semi :: Doc

    For example, we can use these functions to define the `Bop` instance of the `PP` class.
    This instance converts each binary operator into a document.

> instance PP Bop where
>   pp Plus   = PP.char '+'
>   pp Minus  = PP.char '-'
>   pp Times  = PP.char '*'
>   pp Divide = PP.char '/'
>   pp Gt     = PP.char '>'
>   pp Ge     = PP.text ">="
>   pp Lt     = PP.char '<'
>   pp Le     = PP.text "<="

  * Combinators for combining Docs in various ways, providing constraints on the textual layout.
    For example, some are listed below. (See the library documentation for *many* more.)

     -- Beside. Combines two documents horizontally with no space between.
     (<>) :: Doc -> Doc -> Doc

     -- Beside, separated by space, unless one of the arguments is `empty`.
     (<+>) :: Doc -> Doc -> Doc

     -- Nest (or indent) a document by a given number of positions
     -- (which may also be negative).
     nest :: Int -> Doc -> Doc

     -- Above. Combines two documents vertically (with overlap if
     -- possible: if the last line of the first argument stops at
     -- least one position before the first line of the second begins,
     -- these two lines are overlapped).
     ($$) :: Doc -> Doc -> Doc

     -- List version of $$.
     vcat :: [Doc] -> Doc

     -- wrap document in (..)
     parens :: Doc -> Doc
  
  * Operations for rendering, or converting a Doc to text at the top level. The rendering
    functions are parameterized over display options, such as the maximum line length, so that
    they can figure out how to best display the text. For example, after you finish this problem,
    you should be able to produce the output above by calling the following two functions on `wFact`.

> oneLine :: PP a => a -> String
> oneLine = PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp
> indented :: PP a => a -> String
> indented = PP.render . pp

Your job is to fill in the definitions of `pp` for `Value`s, `Expression`s, `Statement`s and `Block`s.
Note that you should let the pretty printer make decisions about line breaks (i.e. `oneLine` vs.
`indented` above). To do this you should never insert explicit newline and space characters \-\-
use the combinators above instead.

> instance PP Value where
>   pp _ = error "TBD: pretty printing values"

> instance PP Expression where
>   pp _ = error "TBD: pretty printing expressions"

> instance PP Block where
>   pp _ = error "TBD: pretty printing blocks"

> instance PP Statement where
>   pp _ = error "TBD: pretty printing statements"

Your code for your pretty printer should pass the tests in [Tests.hs][tesths] for displaying values,
expressions, and statement blocks. This concrete syntax resembles C or Java, but there are
some simplifications! Take a look at the testcases to figure out how this abstract syntax
should be displayed.

Make sure to add your own tests.

> -- use the C++ precendence level table
> level :: Bop -> Int
> level Times  = 7
> level Divide = 7
> level Plus   = 5
> level Minus  = 5
> level _      = 3    -- comparison operators

A Stepper for WHILE
----------------------

Now that we have a way to display programs nicely, we can implement a "debugger" that we can
use to step through the evaluation of imperative programs.

The first part of this definition is a function that will partially evaluate a block of
statements. The `step` function (that you will write) below should do "one-step" of evaluation.

> step :: Block -> State Store Block
> step _ = undefined

Note that this `step` function should always terminate, even if the input program does not.
For examples of how `step` should operate, see the `Test` file.

Iterating this `step` function until we do not have any more statements to execute, should
produce an alternative interpreter for the language. The `execState` function below should
behave exactly the same as exec above.

> -- | Is this block completely evaluated?
> final :: Block -> Bool
> final (Block []) = True
> final _          = False

> -- | Evaluate this block to completion
> execStep :: Block -> Store -> Store
> execStep = undefined

> -- | Evaluate this block for a specified number of steps
> boundedStep :: Int -> Block -> State Store Block
> boundedStep = undefined

Finally, we can use the step function in a "stepper", an interactive program that lets us
observe the computation piece-by-piece.

For example, here\'s an interaction with the factorial function.... Each line beginning
with `imp>` is a prompt for the tool, allowing the user to type commands such as `n` (step to next statement),
`v x` examine the value of variable `x`, or `b` (step backwards). Lines beginning with `-->` show
the current step of the computation (before it has been executed).

       *Main> stepper wFact
       -->n = 5;                               -- ready to execute first assignment
       imp> n
       -->f = 1;                               -- "next" ready to executed second assignment
       imp> n
       -->while (n > 0) {                      -- now ready for the while loop
         x = n;
         z = f;
         while (x > 1) {
           f = z + f;
           x = x - 1;
         }
         n = n - 1;
       }
       imp> n                                 -- inside the while loop
       -->x = n;
       imp> v f                               -- look up the value of a variable (f)
       1
       -->x = n;
       imp> v n                               -- look up another variable (n)
       5
       -->x = n;
       imp> v x                               -- we haven't yet assigned to x
       Unknown var
       -->x = n;
       imp> b                                 -- we can go backwards!
       -->while (n > 0) {
         x = n;
         z = f;
         while (x > 1) {
           f = z + f;
           x = x - 1;
         }
         n = n - 1;
       }
       imp> b                                 -- another backwards step
       -->f = 1;
       imp> v f                               -- now the variable is undefined
       Unknown var
       -->f = 1;

Now edit the following stepping function so that it has the behavior shown above.
You can change the interface of the local go function. However, do not change the
type of `stepper`.

> stepper :: Block -> IO ()
> stepper b = go b where
>   go block  = do
>     putBlock block
>     putStr "imp> "
>     str <- getLine
>     case str of
>       "x" -> return ()    -- quit the stepper
> 
>       _   -> putStrLn "?" >> go block -- unknown command
>   putBlock :: Block -> IO ()
>   putBlock (Block [])    = putStrLn "done"
>   putBlock (Block (s:_)) = putStr   "-->"   >> putStrLn (PP.render (pp s))

A Parser for WHILE
-------------------

The dual problem to pretty printing is parsing. For this part of the assignment,
you will practice with the parser combinators that we discussed in class.

This part of the assignment uses the definition of the \"Parser\" type from [Parser.hs][parserhs].
This module is augmented by the combinators from [ParserCombinators.hs][parsercombhs]. You should
read over these modules before continuing.

Again, the concrete syntax of the language resembles C or Java, but is a variant of those languages.
Your parser does not have to be able to parse all of C; only the test cases that we provide.
For example, C allows \"if\" statements to [omit the \"else\" branch](https://en.wikipedia.org/wiki/Dangling_else);
but WHILE does not.
Furthermore, \"if\" statements in C may omit braces, but they are
[always required here](https://nakedsecurity.sophos.com/2014/02/24/anatomy-of-a-goto-fail-apples-ssl-bug-explained-plus-an-unofficial-patch/).

**Parsing Constants**

First, we will write parsers for the Value type

> valueP :: P.Parser Value
> valueP = intP <|> boolP

To do so, fill in the implementation of

> intP :: P.Parser Value
> intP = error "TBD"

Next, define a parser that will accept a particular string `s` as a given value `x`

> constP :: String -> a -> P.Parser a
> constP _ _ = error "TBD"

and use the above to define a parser for boolean values where `"true"` and `"false"` should be parsed appropriately.

> boolP :: P.Parser Value
> boolP = error "TBD"

Continue to use the above to parse the binary operators

> opP :: P.Parser Bop
> opP = error "TBD"

**Parsing Expressions**

Next, the following is a parser for variables, where each variable is one-or-more lowercase letters.

> varP :: P.Parser Variable
> varP = some P.lower

Now define a parser combinator which takes a parser, runs it, then skips over any whitespace
characters occurring afterwards

> wsP :: P.Parser a -> P.Parser a
> wsP p = error "TBD"

Use the above to write a parser for `Expression` values

> exprP :: P.Parser Expression
> exprP = error "TBD"

[Tests.hs][tesths] contains some tests for your expression parser. Of course, you will need to add
some test cases of your own parser to that file. In particular, be sure to make sure that your
parser succeeds even when there is white space *within* and *after* an expression.

**Parsing Statements**

Next, use the expression parsers to a statement parser

> statementP :: P.Parser Statement
> statementP = error "TBD"

and one for the sequence of statements at the toplevel. (These should not be surrounded by braces.)

> toplevelP :: P.Parser Block
> toplevelP = error "TBD"

Don't forget to test your parser!