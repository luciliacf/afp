---
title: Exercise: Kata code review - comments
date: September 20, 2017
---

Questions to discuss and answer with your partner:

 1. Which of these three answers is the most readable to you right now? Why?

 2. The general structure of the problem is to read in data from a test file and
    then calculate with that data. In which of these three versions is that
    structure the most apparent?

 3. There is a design decision even in this small example: Should the program
    specify the lines of interest (and how?), or should the line parser just
    ignore lines that don\'t make sense?  Which is more robust?

 4. How could these examples be improved? (pick one and improve it.)

> module Kata where

> import           Data.Char  as Char
> import           Data.List  as List
> import qualified Data.Maybe as Maybe
> import           Test.HUnit
> import qualified Text.Read  as Read


> -- SAMPLE A --
>
> (|>) :: a -> (a -> b) -> b
> (|>) = flip ($)
>
> -- / Applies a function to the value in a Maybe if the value exists
> maybeMap :: (a -> b) -> Maybe a -> Maybe b
> maybeMap _ Nothing  = Nothing
> maybeMap f (Just x) = Just $ f x
>
> -- / Applies a function to the value in two Maybes if both values exist
> maybeMap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
> maybeMap2 _ Nothing  _        = Nothing
> maybeMap2 _ _        Nothing  = Nothing
> maybeMap2 f (Just x) (Just y) = Just $ f x y
>
> -- / Reads an Int from a String without requiring typecasting.
> readInt :: String -> Maybe Int
> readInt = Read.readMaybe
>
> -- / Function that finds the smallest absolute difference between two numbers
> -- extracted from a string. The function to extract the relevant numbers and
> -- the number of initial lines to skip must be provided.
> smallestDifference :: ([String] -> Maybe (String, String, String))
>                        -> Int -> String -> String
> smallestDifference lineExtractor skipLines str =
>   lines str                                  -- [String]
>     |> drop skipLines                        -- [String]
>     |> map words                             -- [[String]]
>     |> map (processRawData . lineExtractor)  -- [Maybe (Int, String)]
>     |> Maybe.catMaybes                       -- [(Int, String)]
>     |> minimum                               -- (Int, String)
>     |> snd                                   -- String
>     |> dropWhile (==' ')                     -- String
>
> -- / Finds the absolute difference between Integers in s2 and s3. If data
> -- exists.
> processRawData :: Maybe (String, String, String) -> Maybe (Int, String)
> processRawData (Just (s1, s2, s3)) =
>   let i1 = readInt s2 in
>   let i2  = readInt s3 in
>   maybeMap2 (\x y -> abs $ x - y) i1 i2
>     |> maybeMap (\x -> (x, s1))
> processRawData _ = Nothing
>
> soccer2_A :: String -> String
> soccer2_A = smallestDifference soccerExtractor 1
>
> soccerExtractor :: [String] -> Maybe (String, String, String)
> soccerExtractor (_:name:_:_:_:_:ptsFor:_:ptsAgainst:_) =
>   Just (name, ptsFor, ptsAgainst)
> soccerExtractor _                                      = Nothing
>
> weather2_A :: String -> String
> weather2_A = smallestDifference weatherExtractor 18
>
> weatherExtractor :: [String] -> Maybe (String, String, String)
> weatherExtractor (day:high:low:_) = Just (day, high, low)
> weatherExtractor _                = Nothing


> -- SAMPLE B --
>
> readInt2 :: String -> Int
> readInt2 s = case readInt s of
>         (Just a) -> a
>         Nothing  -> error ("Cannot parse" ++ s)
>
> selectLines :: Int -> Int -> [a] -> [a]
> selectLines startL endL l = drop (startL-1) (take endL l)
>
> getDiffs :: Int -> Int -> [[String]] -> [Int]
> getDiffs indexX indexY = map (\l -> readInt2 (l !! indexX) - readInt2 (l !! indexY))
>
> getMinIndices :: [Int] -> [Int]
> getMinIndices l = List.elemIndices (minimum l) l
>
> weather2_B :: String -> String
> weather2_B str =
>   let l = map words (selectLines 19 49 (lines str))
>       index = head (getMinIndices (getDiffs 1 2 l))
>   in (l !! index) !! 0
>
> soccer2_B :: String -> String
> soccer2_B str =
>   let l = map words (selectLines 2 18 (lines str) ++ selectLines 20 22 (lines str))
>       index = head (getMinIndices (map abs (getDiffs 6 8 l)))
>   in (l !! index) !! 1
>

> -- SAMPLE C --
>
> -- | Calculates the difference between the max and the min temperature
> -- | for each line of the data.
> weatherLineProcessor :: [String] -> Int
> weatherLineProcessor (x1 : x2 : x3 : _) =
>     if  Maybe.isJust (readInt x1)
>         && readInt x2 >= Just 10
>         && readInt x3 >= Just 10
>     then Maybe.fromJust (readInt x2) - Maybe.fromJust (readInt x3)
>     else 10000
> weatherLineProcessor _ = 10000
>
> soccerLineProcessor :: [String] -> Int
> soccerLineProcessor (x1 : x2 : x3 : x4 : x5: x6 : x7 : x8 : x9 : _) =
>     if  Maybe.isJust (readInt x3)
>         && Maybe.isJust (readInt x4)
>         && Maybe.isJust (readInt x5)
>         && Maybe.isJust (readInt x6)
>     then abs $ Maybe.fromJust (readInt x7) - Maybe.fromJust (readInt x9)
>     else 10000
> soccerLineProcessor _ = 10000
>
> processor :: [String] -> ([String] -> Int) -> String -> Int -> Int -> String
> processor [] f name score idx = name
> processor (x : xs) f name score idx =
>     if f (words x) < score
>     then processor xs f (words x !! idx) (f $ words x) idx
>     else processor xs f name score idx
>
> weather2_C :: String -> String
> weather2_C str = processor (lines str) weatherLineProcessor "" 100 0
>
> soccer2_C :: String -> String
> soccer2_C str = processor (lines str) soccerLineProcessor "" 100 1
>


> -- main
> testWeather :: (String -> String) -> Test
> testWeather weather = "weather" ~: do
>   str <- readFile "jul17.dat"
>   weather str @?= "6"
>
> testSoccer :: (String -> String) -> Test
> testSoccer soccer = "soccer" ~: do
>   str <- readFile "soccer.dat"
>   soccer str @?= "Aston_Villa"
>
> main :: IO ()
> main = do
>   _ <- runTestTT $ TestList [
>        "A" ~: testWeather weather2_A,
>        "B" ~: testWeather weather2_B,
>        "C" ~: testWeather weather2_C,
>        "A" ~: testSoccer soccer2_A,
>        "B" ~: testSoccer soccer2_B,
>        "C" ~: testSoccer soccer2_C ]
>   return ()



Comments made by some students
----------------------------

- All make assumptions about the format of the data, but C makes assumptions about the content of the data.
- A checks for possible nothing. B crashes if it hits something unexpected. And C hard codes in values which could fail if a value was different
- B has short, understandable, general helper functions. You can understand how they might be used before actually looking where they are.
- Sample A is probably more maintainable because the individual functions do not have as many dependencies as the other samples.
- Sample C uses magical numbers/assumptions (like assuming that if num > 10, it\'s a temperature reading)
- A is maybe more difficult to understand even with the help of the comments, while B is comparatively short and straightforward to understand
- I think the solution B makes the process of reading a text file, processing it, and grabbing the data you want much more clear.
- B and C lack documentation.
- Both B and C use a lot of partial functions
- Neat to think of this problem as a series of data transformations, which is what A does.
- B does lots of random access selections on a list, which is less computationally efficient since they\'re lists and not arrays.
- Sample C is too specific with regards to the weather and soccer files. You would not necessarily understand it without having the data files.
- Although B has some hardcoded portions, it\'s nice and concise. If you have access to the files that are being used with solution B, then this code should be clear \-\- however, you won\'t always have access to all inputs so I guess that's a mark against it.
- B and C use partial functions and throw errors, which A does not.
- Sample C doesn\'t use pattern matching and has repetitive calculations
- Some of the functions in B return random large values for bad inputs, which is bad style.
- I like how both A and C created processor functions to encapsulate application specific functionality
- C doesn't seem very safe because it uses `fromJust`. Even though there are error checks, maybe the file is invalid
- C hard codes values and thus won\'t work if given different values.
- B is very straightforward, and involves just as much hard coding of line and column information as A and C.
- C could rework the use of \"magic selectors i.e\", by passing in an anonymous function based on which document is being read.

