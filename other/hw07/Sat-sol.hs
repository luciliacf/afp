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
import Control.Applicative ((<|>))

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

instance Semigroup CNF where
    (Conj c1) <> (Conj c2) = Conj (c1 <> c2)

instance Monoid CNF where
    mempty = Conj mempty   

instance Enum Var where
  toEnum i         = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

allVars :: [ Var ]
allVars = [ vA .. ]

-------------------------------------------------------------------------

-- | The list of variables in a formula
listVars :: CNF -> [Var]
listVars (Conj l) = map var $ concat $ map lits l

-- | The number of times each variable appears in the formula p
countVars :: CNF -> Map Var Int
countVars p = foldr countV Map.empty (listVars p)
    where countV v vmap = case vmap Map.!? v of
                            Nothing -> Map.insert v 1 vmap
                            Just n  -> Map.adjust (+1) v vmap

-- | All of the variables that appear anywhere in the formula, in sorted order
vars :: CNF -> [Var]
vars = sort . nub . listVars

testCountVars :: Test
testCountVars = "countVars" ~:
  countVars exampleFormula ~?= Map.fromList [(vA, 2), (vB, 2), (vC, 2)]

testVars :: Test
testVars = "vars" ~:
   vars exampleFormula ~?= [vA, vB, vC]

-------------------------------------------------------------------------

genVar :: Int -> Gen Var
genVar n = elements (take (abs n + 1) allVars)

genLit :: Int -> Gen Lit
genLit n = Lit <$> arbitrary <*> genVar n

genClause :: Int -> Gen Clause
genClause n = Clause <$> listOf (genLit n)

genCNF :: Int -> Gen CNF
genCNF n = Conj <$> listOf (genClause n)

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

----- some example formulas ---------------------------------------
validFormula :: CNF
validFormula = Conj []

anotherUnsatFormula :: CNF
anotherUnsatFormula = Conj [ Clause [] ]

exampleFormula2 :: CNF
exampleFormula2 = Conj [Clause [Lit False vA, Lit True vB, Lit True vC],
                        Clause [Lit True vA, Lit True vC, Lit True vD],
                        Clause [Lit True vA, Lit True vC, Lit False vD],
                        Clause [Lit True vA, Lit False vC, Lit True vD],
                        Clause [Lit True vA, Lit False vC, Lit False vD],
                        Clause [Lit False vB, Lit False vC, Lit True vD],
                        Clause [Lit False vA, Lit True vB, Lit False vC],
                        Clause [Lit False vA, Lit False vB, Lit True vC]]

exampleValuation2 :: Valuation
exampleValuation2 = Map.fromList [(vA, True), (vB, True), (vC, True), (vD, True)]

-- unsatisfiable
exampleFormula3 :: CNF
exampleFormula3 = Conj [Clause [Lit False vA], Clause []]

exampleFormula4 :: CNF
exampleFormula4 = Conj [Clause [Lit False vA], Clause [Lit True vA, Lit False vA]]

exampleValuation4 :: Valuation
exampleValuation4 = Map.fromList [(vA, False) ]

exampleFormula5 :: CNF
exampleFormula5 = Conj [Clause [Lit True vA, Lit False vB], Clause [Lit False vA],
                        Clause [Lit True vB], Clause [Lit True vB, Lit True vC, Lit True vD] ]

exampleFormula6 :: CNF
exampleFormula6 = Conj [Clause [Lit True vA, Lit False vB], 
                        Clause [Lit True vB, Lit True vC, Lit True vD] ]

---------------------------------------------------------------

testSatisfiedBy :: Test
testSatisfiedBy = "satisfiedBy" ~:  TestList
 [ "exampleFormula" ~:
    assertBool "" (exampleFormula `satisfiedBy` exampleValuation),
   "exampleFormula2" ~:
    assertBool "" (exampleFormula2 `satisfiedBy`exampleValuation2),
   "exampleFormula4" ~: 
    assertBool "" (exampleFormula4 `satisfiedBy` exampleValuation4) ]

prop_unSatBy :: Valuation -> Bool
prop_unSatBy v = not (unSatFormula `satisfiedBy` v)

extend :: Var -> Bool -> Valuation -> Valuation
extend = Map.insert 

value :: Var -> Valuation -> Maybe Bool
value var valuation = valuation Map.!? var

---------------------------------------------------------------------------
-- Simple SAT Solver

type Solver = CNF -> Maybe Valuation

makeValuations :: [Var] -> [Valuation]
makeValuations []     = [ Map.empty ]
makeValuations (v:vs) = [ extend v b vmap | b <- [True,False], vmap <- makeValuations vs ] 

prop_makeValuations :: CNF -> Bool
prop_makeValuations p = length valuations == 2 ^ length ss && allElementsDistinct valuations
  where valuations = makeValuations ss
        ss = vars p

allElementsDistinct :: Eq a => [a] -> Bool
allElementsDistinct []     = True
allElementsDistinct (x:xs) = notElem x xs &&
                             allElementsDistinct xs

sat0 :: Solver
sat0 p = satVal p (makeValuations (vars p))
  where satVal p []       = Nothing
        satVal p (va:vas) = if p `satisfiedBy` va then Just va
                            else satVal p vas 

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
instantiate p v b = Conj $ foldr instClauses [] (clauses p)
   where instClauses c@(Clause []) _  = [c]
         instClauses c@(Clause ls) cs = case instLits ls of
                                          Nothing  -> cs
                                          Just ls' -> (Clause ls'):cs 
         instLits = foldr f (Just []) 
               where f l@(Lit b' v') r
                       | v'==v && b'==b      = Nothing
                       | v'==v && b'== not b = r
                       | otherwise           = fmap (l:) r

         
prop_instantiate :: CNF -> Var -> Bool
prop_instantiate p v = prop_satResult sat0 (instantiate p v True) || prop_satResult sat0 (instantiate p v False)

sat1 :: Solver
sat1 p = sat p
  where sat (Conj [])          = Just Map.empty
        sat (Conj [Clause []]) = Nothing
        sat p                  = fmap (Map.insert v True)  (sat (instantiate p v True)) <|>
                                 fmap (Map.insert v False) (sat (instantiate p v False))
                                    where (v:vs) = vars p
prop_sat1 :: CNF -> Bool
prop_sat1 s = isJust (sat1 s) == isJust (sat0 s)

---------------------------------------------------------------------------
-- Unit propagation

-- 1) If (simplifyUnitClause s) returns Nothing, then there
--    are no remaining unit clauses in s.
-- 2) If it returns (Just s', v, b), then s' is satisfiable iff s is.
prop_simplifyUnitClause :: CNF -> Bool
prop_simplifyUnitClause p = case simplifyUnitClause p of
                              Nothing       -> unitClauses p == []
                              Just (p',v,b) -> isJust (sat0 p) == isJust (sat0 p')

-- returns the list of unit clauses of the given formula
unitClauses :: CNF -> [Lit]
unitClauses (Conj l) = foldr getUnitClause [] l
   where getUnitClause (Clause [Lit b v]) r = (Lit b v):r
         getUnitClause _                  r = r

simplifyUnitClause :: CNF -> Maybe (CNF, Var, Bool)
simplifyUnitClause p = case unitClauses p of
                         []             -> Nothing
                         ((Lit b v):ls) -> Just (instantiate p v b, v, b)

-- If the formula is either satisfied or falsified, return an appropriate answer as before.
--
-- If there are any unit clauses, choose one, simplify it, and call the solver
-- recursively on the simplified formula. If the recursive call returns a satisfying valuation,
-- add a binding for the variable that was eliminated when we simplified the unit clause and return it.
--
-- Otherwise, instantiate one of the variables in the formula and recurse, as before.

sat2 :: Solver
sat2 = sat
  where sat (Conj [])          = Just Map.empty
        sat (Conj [Clause []]) = Nothing
        sat p                  = case simplifyUnitClause p of
                                   Nothing       -> fmap (Map.insert v True)  (sat (instantiate p v True)) <|>
                                                    fmap (Map.insert v False) (sat (instantiate p v False))
                                                       where v = head (vars p)
                                   Just (p',v,b) -> fmap (Map.insert v b) (sat p')  

prop_sat2 :: CNF -> Bool
prop_sat2 s = isJust (sat2 s) == isJust (sat0 s)

---------------------------------------------------------------------------
-- Pure literal elimination

-- 1) If (simplifyPureLiteral s) returns Nothing, then there
--    are no remaining pure literals in s
-- 2) If it returns (Just s'), then s' is satisfiable iff s is
prop_simplifyPureLiteral :: CNF -> Bool
prop_simplifyPureLiteral p = case simplifyPureLiteral p of
                               Nothing       -> pureLiterals p  == []
                               Just (p',v,b) -> isJust (sat1 p) == isJust (sat1 p')

fLits :: CNF -> [Lit]
fLits (Conj l) = foldr ((++) . lits) [] l

pureLiterals :: CNF -> [(Var,Bool)]
pureLiterals p = (Set.toList . snd) $ foldr f (Set.empty, Set.empty) (fLits p)
   where f :: Lit -> (Set Var, Set (Var,Bool)) -> (Set Var, Set (Var,Bool))
         f (Lit b v) (visited, pure)
            | not (Set.member v visited) = (Set.insert v visited, Set.insert (v,b) pure)
            | Set.member (v, not b) pure = (visited, Set.delete (v, not b) pure)
            | otherwise                  = (visited, pure)
            
simplifyPureLiteral :: CNF -> Maybe (CNF, Var, Bool)
simplifyPureLiteral p = case pureLiterals p of
                         []         -> Nothing
                         ((v,b):ps) -> Just (instantiate p v b, v, b)


-- The final DPLL algorithm:
dpll :: Solver
dpll = sat
  where sat (Conj [])          = Just Map.empty
        sat (Conj [Clause []]) = Nothing
        sat p                  = case (simplifyUnitClause p <|> simplifyPureLiteral p) of
                                   Nothing       -> fmap (Map.insert v True)  (sat (instantiate p v True)) <|>
                                                    fmap (Map.insert v False) (sat (instantiate p v False))
                                                       where v = head (vars p)
                                   Just (p',v,b) -> fmap (Map.insert v b) (sat p')  


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
