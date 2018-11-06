---------------------------------------------------------------------------

{-# OPTIONS -fwarn-incomplete-patterns -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}


module Sat where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List as List
import Data.Maybe as Maybe
import Data.Char as Char

import Test.HUnit (Test(..), (~:), (~?=), runTestTT, assertBool)
import Test.QuickCheck



---------------------------------------------------------------------------
-- Basic types

-- | An expression in CNF (conjunctive normal form) is a conjunction
-- of clauses. We also record the number of times each variable appears
-- in the formula

newtype CNF = Conj { clauses :: [Clause] } deriving (Eq, Show)

-- | A clause is a disjunction of a number of literals
newtype Clause = Clause [ Lit ] deriving (Eq, Ord, Show)

-- | A literal is either a positive or a negative variable
data Lit = Lit Bool Var deriving (Eq, Ord, Show)

-- | A variable is just a character
newtype Var = Var Char
  deriving (Eq, Ord, Show)

-- A few variables for test cases
vA, vB, vC, vD :: Var
vA = Var 'A'
vB = Var 'B'
vC = Var 'C'
vD = Var 'D'

exampleFormula :: CNF
exampleFormula = Conj [Clause [Lit True vA, Lit True vB, Lit True vC],
                  Clause [Lit False vA],
                  Clause [Lit False vB, Lit True vC]]

-------------------------------------------------------------------------

-- | Extract the literals from a clause
lits :: Clause -> [Lit]
lits (Clause l) = l

-- | Extract the variable from a literal
var :: Lit -> Var
var (Lit _ x) = x

-- | Is the literal positive?
isPos :: Lit -> Bool
isPos (Lit b _) = b

-- | Negate a literal
neg :: Lit -> Lit
neg (Lit b x) = Lit (not b) x

instance Monoid CNF where
    mempty = Conj mempty
    mappend (Conj c1) (Conj c2) = Conj (mappend c1 c2)

instance Enum Var where
  toEnum i         = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

allVars :: [ Var ]
allVars = [ vA .. ]

-------------------------------------------------------------------------

-- | The number of times each variable appears in the formula
countVars :: CNF -> Map Var Int
countVars = undefined



-- | All of the variables that appear anywhere in the formula, in sorted order
vars :: CNF -> [Var]
vars = undefined

testCountVars :: Test
testCountVars = "countVars" ~:
  countVars exampleFormula ~?= Map.fromList [(vA, 2), (vB, 2), (vC, 2)]

testVars :: Test
testVars = "vars" ~:
   vars exampleFormula ~?= [vA, vB, vC]

-------------------------------------------------------------------------

genVar      :: Int -> Gen Var
genVar    n = elements (take (abs n + 1) allVars)

genLit      :: Int -> Gen Lit
genLit    n = Lit <$> arbitrary <*> genVar n

genClause   :: Int -> Gen Clause
genClause n = Clause <$> listOf (genLit n)

genCNF      :: Int -> Gen CNF
genCNF     n = Conj <$> listOf (genClause n)

defaultNumVariables :: Int
defaultNumVariables = 5

instance Arbitrary Var where
  arbitrary = genVar defaultNumVariables
  shrink v | v == vA   = []
           | otherwise = [ vA .. pred v ]

instance Arbitrary Lit where
  arbitrary = genLit defaultNumVariables
  shrink (Lit b v) = map (flip Lit v) (shrink b) ++
                     map (Lit b) (shrink v)

instance Arbitrary Clause where
   arbitrary = genClause defaultNumVariables
   shrink (Clause l) = [Clause l' | l' <- shrink l]

instance Arbitrary CNF where
   arbitrary = fmap Conj arbitrary
   shrink (Conj x) = [Conj x' | x' <- shrink x]



---------------------------------------------------------------------
-- Satisfiable and unsatisfiable formulae

unSatFormula :: CNF
unSatFormula = Conj [Clause [Lit True vA], Clause [Lit False vA]]

-- | Assignments of values to (some) variables
type Valuation = Map Var Bool

emptyValuation :: Valuation
emptyValuation = Map.empty

fromList :: [(Var,Bool)] -> Valuation
fromList = Map.fromList

exampleValuation :: Valuation
exampleValuation = Map.fromList [(vA, False), (vB, True), (vC, True)]

litSatisfied :: Valuation -> Lit -> Bool
litSatisfied a (Lit b v) = Map.member v a && (b == a Map.! v)

satisfiedBy :: CNF -> Valuation -> Bool
satisfiedBy p a = all (any (litSatisfied a) . lits) (clauses p)

validFormula :: CNF
validFormula = Conj []

anotherUnsatFormula :: CNF
anotherUnsatFormula = Conj [ Clause [] ]

testSatisfiedBy :: Test
testSatisfiedBy = "satisfiedBy" ~:  TestList
 [ "exampleFormula" ~:
    assertBool "" (exampleFormula `satisfiedBy` exampleValuation),
   "another example" ~:
    assertBool "" (error "ADD your own test case here") ]

prop_unSatBy :: Valuation -> Bool
prop_unSatBy v = not (unSatFormula `satisfiedBy` v)

extend :: Var -> Bool -> Valuation -> Valuation
extend = undefined

value :: Var -> Valuation -> Maybe Bool
value = undefined

---------------------------------------------------------------------------
-- Simple SAT Solver

type Solver = CNF -> Maybe Valuation

makeValuations :: [Var] -> [Valuation]
makeValuations = undefined

prop_makeValuations :: CNF -> Bool
prop_makeValuations p = length valuations == 2 ^ length ss
                     && allElementsDistinct valuations where
   valuations = makeValuations ss
   ss = vars p

allElementsDistinct :: Eq a => [a] -> Bool
allElementsDistinct []     = True
allElementsDistinct (x:xs) = notElem x xs &&
                             allElementsDistinct xs

sat0 :: Solver
sat0 = undefined

prop_satResultSound :: Solver -> Int -> Property
prop_satResultSound solver i =
  forAll (genCNF i) $ \p -> case solver p of
                               Just a  -> p `satisfiedBy` a
                               Nothing -> True

unsatisfiable :: CNF -> Bool
unsatisfiable p = all (\a -> not (p `satisfiedBy` a))
  (makeValuations (vars p))

prop_satResult :: Solver -> CNF -> Bool
prop_satResult solver p = case solver p of
                             Just a  -> p `satisfiedBy` a
                             Nothing -> unsatisfiable p

---------------------------------------------------------------------------
-- Instantiation

instantiate :: CNF -> Var -> Bool -> CNF
instantiate = undefined

prop_instantiate :: CNF -> Var -> Bool
prop_instantiate = undefined


sat1 :: Solver
sat1 = sat where
  sat = undefined

prop_sat1 :: CNF -> Bool
prop_sat1 s = isJust (sat1 s) == isJust (sat0 s)

---------------------------------------------------------------------------
-- Unit propagation

simplifyUnitClause :: CNF -> Maybe (CNF, Var, Bool)

-- 1) If (simplifyUnitClause s) returns Nothing, then there
--    are no remaining unit clauses in s.
-- 2) If it returns (Just s'), then s' is satisfiable iff s is.
prop_simplifyUnitClause :: CNF -> Bool
prop_simplifyUnitClause = undefined

unitClauses :: CNF -> [Lit]
unitClauses = undefined

simplifyUnitClause = undefined

sat2 :: Solver
sat2 = sat where
  sat = undefined

prop_sat2 :: CNF -> Bool
prop_sat2 s = isJust (sat2 s) == isJust (sat0 s)

---------------------------------------------------------------------------
-- Pure literal elimination

simplifyPureLiteral :: CNF -> Maybe (CNF, Var, Bool)

-- 1) If (simplifyPureLiteral s) returns Nothing, then there
--    are no remaining pure literals in s
-- 2) If it returns (Just s'), then s' is satisfiable iff s is
prop_simplifyPureLiteral :: CNF -> Bool
prop_simplifyPureLiteral = undefined



pureLiterals :: CNF -> [(Var,Bool)]
pureLiterals = undefined

simplifyPureLiteral = undefined

-- The final DPLL algorithm:
dpll :: Solver
dpll = sat where
  sat = undefined

prop_dpll :: CNF -> Bool
prop_dpll s = isJust (dpll s) == isJust (sat0 s)

------------------------------------------------------------------------------
-- All the tests in one convenient place:

quickCheckN :: Testable prop => Int -> prop -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }

main :: IO ()
main = do
    putStrLn "Unit tests:"
    runTestTT $ TestList [testCountVars, testVars, testSatisfiedBy]
    putStrLn "Quickcheck properties:"
    quickCheckN 500 prop_unSatBy
    quickCheckN 500 $ prop_satResultSound sat0 defaultNumVariables
    quickCheckN 500 $ prop_satResult      sat0
    quickCheckN 500 prop_instantiate
    quickCheckN 500 prop_sat1
    quickCheckN 500 $ prop_satResultSound sat1
    quickCheckN 500 $ prop_satResult      sat1
    quickCheckN 500 prop_simplifyUnitClause
    quickCheckN 500 prop_sat2
    quickCheckN 500 $ prop_satResultSound sat2
    quickCheckN 500 $ prop_satResult      sat2
    quickCheckN 500 prop_simplifyPureLiteral
    quickCheckN 500 prop_dpll
    quickCheckN 500 $ prop_satResultSound dpll
    quickCheckN 500 $ prop_satResult      dpll
