---
title: Using QuickCheck to develop a SAT solver
author: Lucilia Figueiredo
---

[saths]: Sat.hs
[sat-sol]: ../code/hw07.zip
[DPLL]: https://en.wikipedia.org/wiki/DPLL_algorithm

For this module, please edit the file [Sat.hs][saths]. A solution for this homework will eventually be
available [here][sat-sol]. 

The [Davis-Putnam-Logemann-Loveland][DPLL] algorithm is a method for deciding the satisfiability of
propositional logic formulae. Although the SAT problem is NP-complete, it is still remarkably amenable
to automation, and high-performance SAT-solvers are heavily used in modern software verification,
constraint solving, and optimization. Your task in this problem is to implement the DPLL algorithm
and check your implementation using QuickCheck.

We\'ll lead you through the main steps, but if you\'re not already familiar with the basics of SAT
solving you will need to do a little reading about the basic ideas in DPLL.

 * [DPLL Wikipedia page][DPLL]

Throughout, try to use library functions to make your code short and elegant.
None of the requested function bodies should take much more than a dozen or so lines.
Style counts!

> {-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}
> module Sat where

In this problem, we will use two data structures from Haskell\'s standard library,
implementing Finite Sets and Maps. The two import declarations below say:
(1) import the type `Map` from the module `Data.Map` as an unqualified identifier,
so that we can use just `Map a b` as a type; and (2) import everything else from
`Data.Map` as qualified identifiers, written `Map.member`, etc.

> import Data.Map (Map)
> import qualified Data.Map as Map

We will do the same thing with sets.

> import Data.Set (Set)
> import qualified Data.Set as Set

We will also make other library functions available.

> import Data.List as List
> import Data.Maybe as Maybe
> import Data.Char as Char
> import Control.Applicative ((<|>))

Finally, import definitions for QuickCheck.

> import Test.HUnit (Test(..), (~:), (~?=), runTestTT, assertBool)
> import Test.QuickCheck

If you don\'t yet have the QuickCheck library installed on your machine, start by doing this:

   cabal install quickcheck

Basic types
------------

The DPLL algorithm works on formulae that are in Conjunctive Normal Form (CNF), i.e\.
formulae that consist of a conjunction of clauses, where each clause is a disjunction of
literals, i.e\. positive or negative propositional variables. For example,

     (A \/ B \/ C) /\ (not A) /\ (not B \/ C)

is a CNF formula.

> -- | An expression in CNF (conjunctive normal form) is a conjunction
> -- of clauses. We also record the number of times each variable appears
> -- in the formula
> newtype CNF = Conj { clauses :: [Clause] } deriving (Eq, Show)

> -- | A clause is a disjunction of a number of literals
> newtype Clause = Clause [ Lit ] deriving (Eq, Ord, Show)

> -- | A literal is either a positive or a negative variable
> data Lit = Lit Bool Var deriving (Eq, Ord, Show)

> -- | A variable is just a character
> newtype Var = Var Char
>   deriving (Eq, Ord, Show)

> -- A few variables for test cases
> vA, vB, vC, vD :: Var
> vA = Var 'A'
> vB = Var 'B'
> vC = Var 'C'
> vD = Var 'D'

Here\'s how the formula from above is represented:

> exampleFormula :: CNF
> exampleFormula = Conj [Clause [Lit True vA, Lit True vB, Lit True vC],
>                   Clause [Lit False vA],
>                   Clause [Lit False vB, Lit True vC]]


The next few operations allow us to work with formulae, clauses, literals and variables.

> -- | Extract the literals from a clause
> lits :: Clause -> [Lit]
> lits (Clause l) = l

> -- | Extract the variable from a literal
> var :: Lit -> Var
> var (Lit _ x) = x

> -- | Is the literal positive?
> isPos :: Lit -> Bool
> isPos (Lit b _) = b

> -- | Negate a literal
> neg :: Lit -> Lit
> neg (Lit b x) = Lit (not b) x

Formulae also form a Monoid:

> instance Semigroup CNF where
>     (Conj c1) <> (Conj c2) = Conj (c1 <> c2)
> 
> instance Monoid CNF where
>     mempty = Conj mempty   

Variables are enumerable. However, only the first 26 will print nicely.

> instance Enum Var where
>   toEnum i         = Var (toEnum (i + fromEnum 'A'))
>   fromEnum (Var v) = fromEnum v - fromEnum 'A'
> allVars :: [ Var ]
> allVars = [ vA .. ]

Sometimes we need to know about all of the variables that appear in a particular formulae.
We can use a finite map structure to calculate this information. (You\'ll need to refer to
the documentation for the `Data.Map` module to complete this part.)

> -- | The number of times each variable appears in the formula
> countVars :: CNF -> Map Var Int
> countVars = undefined

> -- | All of the variables that appear anywhere in the formula, in sorted order
> vars :: CNF -> [Var]
> vars = undefined

Here are two test cases, using the example formula above.
Make sure to add a few of your own unit tests, too!

> testCountVars :: Test
> testCountVars = "countVars" ~:
>   countVars exampleFormula ~?= Map.fromList [(vA, 2), (vB, 2), (vC, 2)]

> testVars :: Test
> testVars = "vars" ~:
>    vars exampleFormula ~?= [vA, vB, vC]

Of course, most of the testing we will do in this homework will use QuickCheck.

To do that, we need to be able to generate arbitrary formulae. The following generators
are parameterized by the number of distinct variables to use each formula.
When you are testing solvers below, you\'ll find that changing this number affects the
efficiency of certain solvers and also the distribution of satisfiable random formulae.

> genVar      :: Int -> Gen Var
> genVar    n = elements (take (abs n + 1) allVars)

> genLit      :: Int -> Gen Lit
> genLit    n = Lit <$> arbitrary <*> genVar n

> genClause   :: Int -> Gen Clause
> genClause n = Clause <$> listOf (genLit n)

> genCNF      :: Int -> Gen CNF
> genCNF     n = Conj <$> listOf (genClause n)

We use these generators in our `Arbitrary` instances.

> defaultNumVariables :: Int
> defaultNumVariables = 5

> instance Arbitrary Var where
>   arbitrary = genVar defaultNumVariables
>   shrink v | v == vA   = []
>            | otherwise = [ vA .. pred v ]

> instance Arbitrary Lit where
>   arbitrary = genLit defaultNumVariables
>   shrink (Lit b v) = map (flip Lit v) (shrink b) ++
>                      map (Lit b) (shrink v)

> instance Arbitrary Clause where
>    arbitrary = genClause defaultNumVariables
>    shrink (Clause l) = [Clause l' | l' <- shrink l]

> instance Arbitrary CNF where
>    arbitrary = fmap Conj arbitrary
>    shrink (Conj x) = [Conj x' | x' <- shrink x]


Satisfiable and unsatisfiable formulae
---------------------------------------

Our example formula is said to be satisfiable because it is possible to find an
assignment of truth values to variables \-\- namely

     A |-> False
     B |-> True
     C |-> True

\-\- that makes the example formula true. On the other hand, this formula

> unSatFormula :: CNF
> unSatFormula = Conj [Clause [Lit True vA], Clause [Lit False vA]]

is unsatisfiable because there is no such assignment.

An assignment of truth values to variables is called a valuation.
(In logic, valuations usually assign a truth value to all variables of a formula.
Here we will do things a little bit differently and define a valuation to be a map
from some variables to truth values.)

> -- | Assignments of values to (some) variables
> type Valuation = Map Var Bool

> emptyValuation :: Valuation
> emptyValuation = Map.empty

> fromList :: [(Var,Bool)] -> Valuation
> fromList = Map.fromList

For instance, the valuation above is represented thus:

> exampleValuation :: Valuation
> exampleValuation = Map.fromList [(vA, False), (vB, True), (vC, True)]

We say that a CNF formula is satisfied by a valuation if the valuation makes the formula true.

> litSatisfied :: Valuation -> Lit -> Bool
> litSatisfied a (Lit b v) = Map.member v a && (b == a Map.! v)

> satisfiedBy :: CNF -> Valuation -> Bool
> satisfiedBy p a = all (any (litSatisfied a) . lits) (clauses p)

Take a moment to look at the definition of `satisfiedBy` and consider the following two formulae:

This first formula is a conjunction of zero clauses, all of which must be satisfied
for the formula to be true. So this formula will be satisfied by any valuation,
including the empty one.

> validFormula :: CNF
> validFormula = Conj []

On the other hand, `anotherUnsatFormula` below is the conjunction of a single clause.
That clause must be satisfied in order for the whole formula to be true.
However, that clause is a disjunction; there must be some true literal in the clause
for it to be satisfied, and there isn\'t. So this formula cannot be satisfied by any valuation.

> anotherUnsatFormula :: CNF
> anotherUnsatFormula = Conj [ Clause [] ]

Let\'s create a few tests for `satisfiedBy`. Our example formula is satisfied by the example
valuation. (Add a few more tests of formulae and their satisfying valuations to the list below.)

> testSatisfiedBy :: Test
> testSatisfiedBy = "satisfiedBy" ~:  TestList
>  [ "exampleFormula" ~:
>     assertBool "" (exampleFormula `satisfiedBy` exampleValuation),
>    "another example" ~:
>     assertBool "" (error "ADD your own test case here") ]

Note that our unsatisfiable formula is not satisfied by ANY valuation.
This is a property that we can check with QuickCheck, because we can generate
arbitrary valuations.

> prop_unSatBy :: Valuation -> Bool
> prop_unSatBy v = not (unSatFormula `satisfiedBy` v)

Valuations support two main operations: extending them with a new binding and checking
what is the value of a variable. Please define them using functions from the `Data.Map`
module. (These definitions are short).

> extend :: Var -> Bool -> Valuation -> Valuation
> extend = undefined

> value :: Var -> Valuation -> Maybe Bool
> value = undefined


Simple SAT Solver
-------------------

A solver is a function that finds a satisfying valuations for a given formula (assuming one exists).

> type Solver = CNF -> Maybe Valuation

We start with a simple combinatorial solver that basically tries all possible valuations
and stops whenever it finds a satisfying valuation.

The first step is to implement the `makeValuations` function that calculates all the
valuations for the given variables.

> makeValuations :: [Var] -> [Valuation]
> makeValuations = undefined

To test your implementation, QuickCheck the following property stating that `makeValuations`
is correct, in the sense that it has the right length ($2^n$, where n is the number of variables
in the set) and all its elements are distinct.

> prop_makeValuations :: CNF -> Bool
> prop_makeValuations p = length valuations == 2 ^ length ss && allElementsDistinct valuations
>    where valuations = makeValuations ss
>          ss = vars p

> allElementsDistinct :: Eq a => [a] -> Bool
> allElementsDistinct []     = True
> allElementsDistinct (x:xs) = notElem x xs &&
>                              allElementsDistinct xs

Your first sat solver should now simply search the list of all valuations for one
that satisfies the given formula:

> sat0 :: Solver
> sat0 = undefined

To check that it works, QuickCheck the property `prop_satResultSound sat0 5`, stating
that a successful result returned by a `sat0` on formulae containing five variables is
always a satisfying valuation.

> prop_satResultSound :: Solver -> Int -> Property
> prop_satResultSound solver i =
>   forAll (genCNF i) $ \p -> case solver p of
>                                Just a  -> p `satisfiedBy` a
>                                Nothing -> True

*WARNING:* don\'t QuickCheck this property on `sat0` without specifying the number of
variables (and that number should be small!)

A solver is also complete if whenever it fails to find a satisfying assignment,
then that formula is unsatisfiable. We say that a solver is correct if it is both sound and complete.

> unsatisfiable :: CNF -> Bool
> unsatisfiable p = all (\a -> not (p `satisfiedBy` a))
>   (makeValuations (vars p))

> prop_satResult :: Solver -> CNF -> Bool
> prop_satResult solver p = case solver p of
>                              Just a  -> p `satisfiedBy` a
>                              Nothing -> unsatisfiable p

This property will always be expensive to test, so we separate full correctness from soundness.


Instantiation
--------------

In the remainder of this section, we will gradually build up to an implementation of DPLL.

The simple solver we have just developed is very inefficient. One reason for this
(not the main one, but solving it will set us up for more interesting optimizations later)
is that it evaluates the whole formula every time it tries a different valuation.
Indeed, we can do much better: once we choose a truth value for a propositional variable,
we can simplify the formula to take this choice into account.

For instance, imagine we have the CNF formula

      (A \/ not B) /\ (not A \/ B \/ C)
      
If we instantiate A to True, then the formula becomes `(True \/ not B) /\ (False \/ B \/ C)`,
which can be simplified to `(B \/ C)`.

Please implement the instantiate function:

> instantiate :: CNF -> Var -> Bool -> CNF
> instantiate = undefined

Informally, the correctness property for `instantiate` is that, if `s` is a formula and `v`
a variable, `s` should be satisfiable iff either instantiate `s v True` or instantiate `s v False`
is satisfiable. Use your simple satisfiability checker `sat0` to state this formally as a
QuickCheck property. Use QuickCheck (in GHCi) to test whether your implementation of `instantiate`
has this property.

> prop_instantiate :: CNF -> Var -> Bool
> prop_instantiate = undefined

Now use `instantiate` to write a very simple sat solver that, at each step, chooses a
variable and recursively tries instantiating it with both `True` and `False`.
The algorithm should proceed like this:

First, check if the formula is either satisfied (returning an empty valuation if so)
or falsified (returning `Nothing`).

Otherwise, choose one of the variables in the formula, instantiate it with both `True` and `False`,
and see if either of the resulting formulae are satisfiable. If so, add an appropriate binding
for the variable we instantiated to the resulting `Valuation` and return it.

> sat1 :: Solver
> sat1 = sat where
>   sat = undefined

To check that it works, QuickCheck the property `prop_satResultSound sat1`, `prop_satResult sat1`,
plus this one (and any others that you can think of).

> prop_sat1 :: CNF -> Bool
> prop_sat1 s = isJust (sat1 s) == isJust (sat0 s)


Unit propagation
-----------------

The first significant optimization performed by DPLL is simplifying unit clauses.
Please see the Wikipedia page for a general sense of how it should work.

The signature of the unit clause simplifier is this:

> simplifyUnitClause :: CNF -> Maybe (CNF, Var, Bool)

It should take a formula, choose a unit clause (or return `Nothing` if there aren\'t any),
and simplify it, returning the simplified formula together with the variable that was
instantiated away in the process and the value that it was given.

This time, let\'s start by writing down the correctness property for `simplifyUnitClause`:

> -- 1) If (simplifyUnitClause s) returns Nothing, then there
> --    are no remaining unit clauses in s.
> -- 2) If it returns (Just s'), then s' is satisfiable iff s is.
> prop_simplifyUnitClause :: CNF -> Bool
> prop_simplifyUnitClause = undefined

Now let\'s implement the simplifier. First, let\'s just find all the unit clauses in a formula.
For example, the unit clauses of

     (A \/ not B) /\ (not A) /\ (B) /\ (B \/ C \/ D)

are not A and B, while

     (A \/ not B) /\ (not A \/ B \/ C)

has no unit clauses.

> unitClauses :: CNF -> [Lit]
> unitClauses = undefined
> simplifyUnitClause = undefined

Use QuickCheck to make sure your implementation satisfies the property you wrote.

Next, we can improve the `sat1` solver by inserting a new step:

If the formula is either satisfied or falsified, return an appropriate answer as before.

(New step) If there are any unit clauses, choose one, simplify it, and call the solver
recursively on the simplified formula. If the recursive call returns a satisfying valuation,
add a binding for the variable that was eliminated when we simplified the unit clause and return it.

Otherwise, instantiate one of the variables in the formula and recurse, as before.

Please do so:

> sat2 :: Solver
> sat2 = sat where
>   sat = undefined

To check that it works, QuickCheck the property `prop_satResultSound sat2`, `prop_satResult sat2`
plus this one:

> prop_sat2 :: CNF -> Bool
> prop_sat2 s = isJust (sat2 s) == isJust (sat0 s)


Pure literal elimination
--------------------------

The next (and last) significant optimization is simplifying pure literals.
Again, please see the Wikipedia page for how this works.

> simplifyPureLiteral :: CNF -> Maybe (CNF, Var, Bool)

Again, let\'s write the correctness property first:

> -- 1) If (simplifyPureLiteral s) returns Nothing, then there
> --    are no remaining pure literals in s
> -- 2) If it returns (Just s'), then s' is satisfiable iff s is
> prop_simplifyPureLiteral :: CNF -> Bool
> prop_simplifyPureLiteral = undefined

Now let\'s write the pure literal simplifier (and QuickCheck it!).
For example, the pure literals in the formula

     (A \/ B) /\ (not A) /\ (B) /\ (B \/ C \/ not D)
     
are B, C, and not D.

> pureLiterals :: CNF -> [(Var,Bool)]
> pureLiterals = undefined
> simplifyPureLiteral = undefined

This brings us to the final DPLL algorithm, inserting a new step in `sat2`:

* If the formula is either satisfied or falsified, return an appropriate answer as before.

* If there are any unit clauses, choose one, simplify it, and call the solver recursively on the simplified formula.

* (New step) If there are any pure literals, choose one, simplify it, and call the solver recursively on the simplified formula.

* Otherwise, instantiate one of the variables in the formula and recurse, as before.

> -- The final DPLL algorithm:
> dpll :: Solver
> dpll = sat where
>   sat = undefined

To check that it works, QuickCheck the property `prop_satResultSound dpll`,
`prop_satResultSound dpll`, plus this one:

> prop_dpll :: CNF -> Bool
> prop_dpll s = isJust (dpll s) == isJust (sat0 s)


All the tests in one convenient place
---------------------------------------

> quickCheckN :: Testable prop => Int -> prop -> IO ()
> quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

> main :: IO ()
> main = do
>     putStrLn "Unit tests:"
>     runTestTT $ TestList [testCountVars, testVars, testSatisfiedBy]
>     putStrLn "Quickcheck properties:"
>     quickCheckN 500 prop_unSatBy
>     quickCheckN 500 $ prop_satResultSound sat0 defaultNumVariables
>     quickCheckN 500 $ prop_satResult      sat0
>     quickCheckN 500 prop_instantiate
>     quickCheckN 500 prop_sat1
>     quickCheckN 500 $ prop_satResultSound sat1
>     quickCheckN 500 $ prop_satResult      sat1
>     quickCheckN 500 prop_simplifyUnitClause
>     quickCheckN 500 prop_sat2
>     quickCheckN 500 $ prop_satResultSound sat2
>     quickCheckN 500 $ prop_satResult      sat2
>     quickCheckN 500 prop_simplifyPureLiteral
>     quickCheckN 500 prop_dpll
>     quickCheckN 500 $ prop_satResultSound dpll
>     quickCheckN 500 $ prop_satResult      dpll