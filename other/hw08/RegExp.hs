{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module RegExp where

import Prelude hiding(either)

import Data.Set (Set)
import qualified Data.Set as Set (singleton, fromList, member)

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
split xs = foldr f [] [0..length xs]
   where f n ys = splitAt n xs : ys

-- all decompositions of a string into multi-part (nonempty) pieces
-- parts "abc" = [["abc"],["a","bc"], ["ab","c"], ["a","b","c"]]
parts :: [a] -> [[[a]]]
parts []  = [[]]
parts [x] = [[[x]]]
parts xs  = [ zss <> [ys]  | (zs,ys) <- init $ split xs, zss <- parts zs]

accept :: RegExp -> String -> Bool
accept (Mark _ r)   s = accept r s
accept (Char set) [c] = Set.member c set
accept (Char _)     _ = False
accept (Alt r1 r2)  s = accept r1 s || accept r2 s
accept (Seq r1 r2)  s = any (\(s1,s2) -> accept r1 s1 && accept r2 s2) (split s)    
accept (Star r)     s = null s || any (all (accept r)) (parts s)     
accept Empty        s = null s      
accept Void         _ = False    

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

nestedPat :: RegExp
nestedPat = Mark "word" ((plus lower) `Seq` (Mark "b" (char 'b')) `Seq` (plus lower))

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
        Just (Map.fromList [("word",["a","b","c","d","e"])]),
    patAccept nestedPat "acdbef" ~?=
        Just (Map.fromList [("b", ["b"]),("word",["acdbef"])])
  ]

type Match = Map String [String]

patAccept :: RegExp -> String -> Maybe Match
patAccept r s = patAc r s (Just Map.empty)
  where patAc :: RegExp -> String -> Maybe Match -> Maybe Match
        patAc (Mark tag r) s m = Map.alter f tag <$> patAc r s m
                                 where f Nothing   = Just (s:[])
                                       f (Just ls) = Just (s:ls)
        patAc (Alt r1 r2)  s m = patAc r1 s m <|> patAc r2 s m
        patAc (Seq r1 r2)  s m = foldr ((<|>) . f) Nothing (split s)
                                 where f (s1,s2) = patAc r2 s2 (patAc r1 s1 m)
        patAc (Star r)     s m = if null s then m else Nothing
                                 <|> foldr ((<|>) . foldr (patAc r) m) Nothing (parts s)
        patAc r@(Char set) s m = if accept r s then m else Nothing
        patAc Empty        s m = if null s then m else Nothing     
        patAc Void         _ _ = Nothing    

-- (c)

match :: RegExp -> String -> Bool
match r s = nullable (foldl (deriv . optimize)  r s)

-- | optimize r returs an optimized regular expression equivalent to r
optimize :: RegExp -> RegExp
optimize (Star r)    = rStar r
optimize (Seq r1 r2) = rSeq r1 r2
optimize (Alt r1 r2) = rAlt r1 r2
optimize r           = r

-- | `nullable r` return `True` when `r` matches the empty string
nullable :: RegExp -> Bool
nullable Empty       = True
nullable Void        = False
nullable (Char s)    = False
nullable (Star _)    = True
nullable (Seq r1 r2) = nullable r1 && nullable r2
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Mark _ r)  = nullable r

-- |  Takes a regular expression `r` and a character `c`,
-- and computes a new regular expression that accepts word `w` if `cw` is
-- accepted by `r`.
deriv :: RegExp -> Char -> RegExp
deriv r@(Char set) c = if Set.member c set then Empty else Void
deriv (Alt r1 r2)  c = Alt (deriv r1 c) (deriv r2 c)
deriv (Seq r1 r2)  c = (deriv r1 c `Seq` r2) `Alt` (if nullable r1 then deriv r2 c else Void)     
deriv (Star r)     c = deriv r c `Seq` Star r     
deriv Empty        _ = Void     
deriv Void         _ = Void    
deriv (Mark _ r)   c = deriv r c

testMatch :: Test
testMatch = TestList [
   not (match Void "a") ~? "nothing is void",
   not (match Void "") ~? "really, nothing is void",
   match Empty "" ~? "accept Empty true",
   not (match Empty "a") ~? "not accept Empty",
   match lower "a" ~? "accept lower",
   not (match lower "A") ~? "not accept lower",
   match boldHtml "<b>cis552</b>!</b>" ~? "cis552!",
   not (match boldHtml "<b>cis552</b>!</b") ~? "no trailing" ]

-- (d)

rStar :: RegExp -> RegExp
rStar (Star r) = Star r   -- two iterations is the same as one
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
rSeq Empty r        = r
rSeq r Empty        = r
rSeq Void r         = Void
rSeq r Void         = Void
rSeq r1 r2          = Seq r1 r2

prop_rSeq :: RegExp -> Property
prop_rSeq r = prop_rSeq1 r .&&. prop_rSeq2 r .&&. prop_rSeq3 r .&&. prop_rSeq4 r where
  prop_rSeq1 r = rSeq Empty r %==% Seq Empty r
  prop_rSeq2 r = rSeq r Empty %==% Seq r Empty
  prop_rSeq3 r = rSeq Void r  %==% Seq Void r
  prop_rSeq4 r = rSeq r Void  %==% Seq r Void

rAlt :: RegExp -> RegExp -> RegExp
rAlt Void r  = r
rAlt r Void  = r
rAlt r1 r2   = Alt r1 r2

prop_rAlt :: RegExp -> Property
prop_rAlt r = prop_rAlt1 r .&&. prop_rAlt2 r where
  prop_rAlt1 r = rAlt Void r %==% Alt Void r
  prop_rAlt2 r = rAlt r Void %==% Alt r Void