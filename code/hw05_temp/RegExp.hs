{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module RegExp where

import Prelude hiding(either)

import Data.Set (Set)
import qualified Data.Set as Set (singleton, fromList)

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Applicative(Alternative(..))
import Control.Monad (ap, liftM2)



import Test.HUnit hiding (State)
import Test.QuickCheck
import Test.QuickCheck.Function

main :: IO ()
main = return ()

data RegExp = Char (Set Char)      -- single literal character
            | Alt RegExp RegExp    -- r1 | r2   (alternation)
            | Seq RegExp RegExp    -- r1 r2     (concatenation)
            | Star RegExp          -- r*        (Kleene star)
            | Empty                -- ε, accepts empty string
            | Void                 -- ∅, always fails
            | Mark String RegExp   -- (for marked subexpressions, see (b) below)
  deriving (Show, Eq)

char :: Char -> RegExp
char = Char . Set.singleton

chars :: String -> RegExp
chars = Char . Set.fromList

lower, upper, letter, digit, punc, white, anyc, anyc':: RegExp
lower  = chars ['a' .. 'z']
upper  = chars ['A' .. 'Z']
digit  = chars ['0' .. '9']
punc   = chars "<>!/.*()?@"
white  = chars " \n\r\t"

anyc'  = lower `Alt` upper `Alt` digit `Alt` punc `Alt` white

anyc = chars $ ['a' .. 'z']
               ++ ['A' .. 'Z']
               ++ ['0' .. '9']
               ++ "<>!/.*()?@"
               ++ "\n\r\t"

letter = chars $ ['A' .. 'Z'] ++ ['a' .. 'z']

word :: String -> RegExp
word = foldr (Seq . char) Empty

cis552 :: RegExp
cis552 = word "cis552"

boldHtml :: RegExp
boldHtml = word "<b>" `Seq` Star anyc `Seq`  word "</b>"

plus :: RegExp -> RegExp
plus pat = pat `Seq` Star pat

-- (a)

-- all decompositions of a string into two different pieces
--     split "abc" == [("","abc"),("a","bc"),("ab","c"),("abc","")]
split :: [a] -> [([a], [a])]
split = error "split: unimplemented"

-- all decompositions of a string into multi-part (nonempty) pieces
-- parts "abc" = [["abc"],["a","bc"], ["ab","c"], ["a","b","c"]]
parts :: [a] -> [[[a]]]
parts = error "parts: unimplemented"

accept :: RegExp -> String -> Bool

accept (Mark _ r)  s = accept r s
accept _           _ = error "accept: finish me"

testAccept :: Test
testAccept = TestList [
   not (accept Void "a") ~? "nothing is void",
   not (accept Void "") ~? "really, nothing is void",
   accept Empty "" ~? "accept Empty true",
   not (accept Empty "a") ~? "not accept Empty",
   accept lower "a" ~? "accept lower",
   not (accept lower "A") ~? "not accept lower",
   accept boldHtml "<b>cis552</b>!</b>" ~? "cis552!",
   not (accept boldHtml "<b>cis552</b>!</b") ~? "no trailing" ]

-- (b)

boldHtmlPat :: RegExp
boldHtmlPat = word "<b>" `Seq` Mark "<b>" (Star anyc) `Seq` word "</b>"

namePat :: RegExp
namePat = Mark "first" (plus letter) `Seq` Star white `Seq` Mark "last" (plus letter)

wordsPat :: RegExp
wordsPat = Star (Mark "word" (plus lower) `Seq` Star white)

testPat :: Test
testPat = TestList [
    patAccept boldHtmlPat "<b>cis552" ~?= Nothing,
    patAccept boldHtmlPat "<b>cis552!</b>" ~?=
        Just (Map.fromList [("<b>",["cis552!"])]),
    patAccept boldHtmlPat "<b>cis552</b>!</b>" ~?=
        Just (Map.fromList [("<b>",["cis552</b>!"])]),
    patAccept namePat "Haskell  Curry" ~?=
        Just (Map.fromList [("first",["Haskell"]),("last",["Curry"])]),
    patAccept wordsPat "a    b c   d e" ~?=
        Just (Map.fromList [("word",["a","b","c","d","e"])])
  ]

type Match = Map String [String]

patAccept :: RegExp -> String -> Maybe Match
patAccept = error "patAccept: unimplemented"



-- (c)

match :: RegExp -> String -> Bool
match r s = nullable (foldl deriv r s)

-- | `nullable r` return `True` when `r` matches the empty string
nullable :: RegExp -> Bool
nullable _ = error "nullable: unimplemented"

-- |  Takes a regular expression `r` and a character `c`,
-- and computes a new regular expression that accepts word `w` if `cw` is
-- accepted by `r`.
deriv :: RegExp -> Char -> RegExp
deriv = error "deriv: unimplemented"



-- (d)

rStar :: RegExp -> RegExp
rStar (Star x) = Star x   -- two iterations is the same as one
rStar Empty    = Empty    -- iterating the empty string is the empty string
rStar Void     = Empty    -- zero or more occurrences of void is empty
rStar r        = Star r   -- no optimization

(%==%) :: RegExp -> RegExp -> Property
r1 %==% r2 = forAll (resize 10 (listOf (elements "abcd"))) $
                 \s -> match r1 s == match r2 s

prop_rStar :: RegExp -> Property
prop_rStar r = prop_rStar1 r .&&. prop_rStar2 .&&. prop_rStar3 where
  prop_rStar1 r = rStar (Star r) %==% Star (Star r)
  prop_rStar2   = rStar Empty    %==% Star Empty
  prop_rStar3   = rStar Void     %==% Star Empty

instance Arbitrary RegExp where
   arbitrary = undefined
   shrink = undefined

rSeq :: RegExp -> RegExp -> RegExp
rSeq = undefined

rAlt :: RegExp -> RegExp -> RegExp
rAlt = undefined

