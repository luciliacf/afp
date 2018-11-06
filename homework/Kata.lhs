---
title: Return of the Data Munging Kata
author: Lucilia Figueiredo
---

[hw04zip]: ../code/hw04.zip
[kata-sol]: ../code/kata-sol.hs
[Cassava]: https://github.com/haskell-hvr/cassava

> {-# OPTIONS -fwarn-tabs -fno-warn-type-defaults -fdefer-type-errors #-}
> {-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
> module Kata where

This problem asks you to revisit the data munging Kata from the first homework assignment,
to see how type classes can also help structure programs.
This time you will need the `soccer.dat` file from before, as well as a new file: `jan.dat`,
a weather data file for January 2017 for Philadelphia. You will edit the file `Kata.hs`.
You can dowload these three files [here][hw04zip] 

A solution for this homework will eventually be available [here][kata-sol].

In this problem, you can use any function in the standard prelude or the following libraries:

> import Data.Char as Char
> import Data.List as List
> import Data.Maybe as Maybe
> import Text.Read  (readMaybe)
> import Test.HUnit

The Punchline
--------------

The goal of this assignment is to develop a library for working with data files like
we saw in the first homework assignment.

The punchline of this problem is that all we need to do to calculate the minimum
temperature spread are the lines in this section. Make sure you read all of the
sections of this problem before changing anything.

The key idea of this library is that we want to define a type that corresponds
to the relevant data from the file

> data Weather = Weather {
>   day :: Int, maxT :: Int, minT :: Int 
>   } deriving (Eq, Show)

and then construct a class instance for that type that specifies how to convert
data row-by-row into that data format.

> -- Access this information from tabulated strings
> -- Note that there are more columns in the file, but we ignore the extras
> -- and only work with the ones that we care about
> instance FromRecord Weather where
>   parseRecord (("DY",dy):("MAX",mx):("MIN",mn):_) = Just Weather <**> dy <**> mx <**> mn
>   parseRecord _            = Nothing

Once we have a data model, we can also define domain specific operations for that type.

> -- compare two weather records based on the difference between their
> -- minimum and maximum temperatures
> cmpDiffTemp :: Weather -> Weather -> Ordering
> cmpDiffTemp w1 w2 = compare (diff w1) (diff w2) where
>     diff w = maxT w - minT w

Finally, we can put it all together by first using the library to decode the
file into our data model and then process that data into a useful result.

> weatherProgram :: IO ()
> weatherProgram = do
>    bytes <- readFile "jan.dat"
>    let wdata :: [Weather]
>        wdata = decode AlignRight bytes          -- parse the weather data
>    let ans = day (minimumBy cmpDiffTemp wdata)  -- compute a stat
>    putStrLn $ "The day with minimum temp change was " ++ show ans  -- day 21
> --   putStrLn $ "Foggy days: " ++ show (foggyDays wevents) -- [2,3,4,5,6,7,11,12,17,18,20,21,22,23,24,26]

The Library
--------------

Now, we develop our general purpose library for processing data files. Your job for
this section is to get weatherProgram above to work. (This design is loosely inspired
by [Cassava][Cassava], a Haskell library for working with CSV data.)

This library should work with \"aligned\" data. Notice that in both `jan.dat` and
`soccer.dat` the first line of the file is a header row.
This header row specifies the offsets of the columns in the remainder of the file.

The header rows can either be aligned with the left or the right sides of the data
in the columns. For example, in the Philadelphia weather file, the columns are all right aligned.

       DY MAX MIN AVG DEP HDD CDD  WTR  ...
       ===============================

        1  51  33  42   8  23   0 0.00
        2  43  32  38   4  27   0 0.40
        3  48  41  45  12  20   0 0.32

       ...
However, in the football file, they are left aligned. For flexibility, our library works
with either sort.

> data Alignment  = AlignLeft | AlignRight

Your first task is to calculate the widths of each column, given an alignment and a header row.
These widths will be used to chop up each row in the data file, and eventually convert it to
more structured data. Your implementation of the splitHeader function should validate the test
cases below. Note that in the second test case, the leading spaces define a column with an empty header.

> testSplitHeader = TestList
>   [ splitHeader AlignRight "  Dy MxT   MnT   AvT   HDDay" ~?=
>       [("Dy",4),("MxT",4),("MnT",6),("AvT",6),("HDDay",8)],
>     splitHeader AlignLeft "       Team            P     W    L" ~?=
>       [("",7),("Team",16),("P",6),("W",5),("L",1)]
>    ]
> -- | Use column headers to determine offsets for each row in a table
> splitHeader :: Alignment -> String -> [(String,Int)]
> splitHeader = undefined

Next, write a function to divide each row according to the calculated widths, trimming extra whitespace.

> splitRow :: [(String,Int)] -> String -> [(String,String)]
> splitRow = undefined

For example:

> testSplitRow = splitRow [("",7),("Team",16),("P",6),("L",5)]
>                         "    1. Arsenal         38    26   " ~?=
>                [("","1."),("Team","Arsenal"),("P","38"),("L","26")]

With these these functions, we can now define a general purpose decoder for data files.
This decoder can produce any sort of result data, as long as there is an instance of the
`FromRecord` class.

There is nothing for you to do for this part, other than make sure that you understand these functions.

> class FromRecord a where
>   -- Convert a list of string elements into row
>   parseRecord :: [(String,String)] -> Maybe a
> -- | Decode a data file into a list of data rows
> -- any rows in the data file that are unparseable are ignored
> decode :: forall a. FromRecord a => Alignment -> String -> [a]
> decode alignment str = Maybe.mapMaybe parseRecord (tabulate str) where
>  tabulate :: String -> [[(String,String)]]
>  tabulate str =
>    case lines str of
>     []      -> []
>     hd:rows -> map (splitRow header) rows where
>          header = splitHeader alignment hd

Field parsing
--------------

We\'ve almost gotten all the ingredients together to back up the punchline from above.
The only bit left is the implementation of `FromRecord` for the `Weather` type.

The first part of this definition is an auxiliary class that we can use to tell the
library how to parse each field of a record (such as `Weather`).

> class FromField a where
>   parseField :: String -> Maybe a

For example, all of the fields of the `Weather` record are `Int`s. Therefore, we need to
define an instance of this class for the `Int` type. For that, we use the `readMaybe` function
from the `Text.Read` library. This overloaded function returns a `Maybe` value parsing failure.

> instance FromField Int where
>   parseField = readMaybe

The implementation of `FromRecord` uses `FromField` implicitly, through the use of the binary
operator `<**>`. This operator, defined below, combines together the various fields in the row.
It uses the `FromField` class to allow the type of each field in the structure to determine how
it should be parsed as a Haskell value.

> infixl 4 <**>
> (<**>) :: FromField a => Maybe (a -> b) -> String -> Maybe b
> Just f  <**> str = f `fmap` parseField str
> Nothing <**> str = Nothing

For example, the `Weather` data constructor has type

~~~~~{.Haskell} 
 Weather :: Int -> Int -> Int -> Weather
~~~~~~~~~~

Now consider the partial application

~~~~~{.Haskell} 
(Just Weather <**>) :: String -> Maybe (Int -> Int -> Weather)
~~~~~~~~~~

This function will, when given a string that parses as a number, pass that number to the
`Weather` data constructor.

~~~~~{.Haskell} 
Just Weather <**> "12" == Just (Weather 12)
~~~~~~~~~~

If the string doesn\'t parse, then the entire result will be Nothing.

~~~~~{.Haskell} 
Just Weather <**> "abc" == Nothing
~~~~~~~~~~

In this way, we can parse each of the arguments to the record, returning `Nothing` if any
of them fail to parse.

Parsing different types of fields
----------------------------------

The `Weather` data constructor only takes `Int` arguments, but `parseRecord` is more general
than that. All we need is an instance of `FromField` to parse other types of data.

For example, `String` fields need no conversion, so they can be returned immediately.

> instance FromField String where
>   parseField = Just

Furthermore, the weather file includes a column (marked WX) for weather events.
We can represent these events with a datatype.

> data Event = Fog | IntenseFog | Thunder | Ice | Hail
>            | FreezingRain | Duststorm | Smoke | BlowingSnow | Tornado
>   deriving (Eq, Ord, Enum, Show)

And parse them according to the legend shown in the data file.
(Fill in this instance yourself so that the test case passes.)

> instance FromField Event where
>    
>    parseField _   = Nothing

> testParseEvent :: Test
> testParseEvent = TestList [ parseField "1" ~?= Just Fog,
>                             parseField "X" ~?= Just Tornado,
>                             parseField "12" ~?= (Nothing :: Maybe Event) ]

As another example, for multiple events on the same day, we can define a newtype to give a
new name for lists of events.

> newtype Events = Events { getEvents :: [Event] }

This newtype allows us to define a special purpose parser that parses each character
individually, and fails if any of them are not valid events.

> instance FromField Events where
>    parseField str = fmap Events (sequence (map (parseField . (:[])) str))

> testParseEvents :: Test
> testParseEvents = TestList [ parseField "14" ~?= Just (Events [Fog,Ice]),
>                              parseField " " ~?= (Nothing :: Maybe Events) ]


Above, we are using the sequence function at type `[Maybe a] -> Maybe [a]`.
This function only produces a list if all of the elements of the input list are `Just`.

~~~~~{.Haskell} 
ghci> sequence [Just 1, Just 2, Nothing, Just 3]
Nothing
ghci> sequence [Just 1, Just 2,  Just 3]
Just [1,2,3]
~~~~~ 

Working with the Library
-------------------------

In the weather data file, the column marked \'DEP\' indicates the difference between the day\'s
average temperature and the usual average temperature for that day. Modify the types above
and fill in the definitions below so that we can also calculate the day where the temperature
is the most unseasonable \.\.\. i.e\. the day with the greatest departure from normal. Moreover,
uncomment the corresponding line in the `weatherProgram`.

> mostUnseasonable :: [Weather] -> Int
> mostUnseasonable = undefined

Modify the `weatherProgram` so that is also outputs the most unseanonable day, by using:

> --   putStrLn $ "The Day most unseasonable was " ++ show (mostUnseasonable wdata)  -- day 12

Next, write a function that returns how many days of the month had some sort of precipitation.
The column marked \'WTR\' contains this information, when that column has a \'T\',
that indicates a trace amount, which should be included in the result.

> numPrecip :: [Weather] -> Int
> numPrecip = undefined

Modify the `weatherProgram` so that is also outputs the number of days that had some sort of precipitation, by using:

> --   putStrLn $ "The number of days with precipitation was " ++ show (numPrecip wdata)  -- 3 days

Finally, you will modify the `weatherProgram` in order to also output the list of all foggy days.
Uncomment the definition below, that defines a new type for parsing only weather events:

> -- data WEvents = WEvents {eday :: Int, wx :: Events} deriving (Eq,Show)

Next, uncomment the below instance definition of typeclass `FromRecord` for this type:

> -- instance FromRecord WEvents where
> --  parseRecord (("DY",dy):("MAX",_):("MIN",_):("AVG",_):("DEP",_):("HDD",_):("CDD",_):("WTR",_):
> --               ("SNW",_):("DPTH",_):("SPD",_):("SPD",_):("DIR",_):("MIN",_):("PSBL",_):("S-S",_):("WX",wx):_) =
> --               Just WEvents <**> dy <**> wx
> --  parseRecord _  = Nothing

Then write a function that returns the list of all foggy days (that is,
the list of days where the event `Fog` or `InteseFog` occurs) : 

> -- foggyDays :: [WEvents] -> [Int]
> -- foggyDays = undefined

Finally, modify the `weatherProgram` so that it also outputs the list of all foggy days, by using:

> --   let wdata :: [Weather]
> --       wdata = decode AlignRight bytes           -- parse the weather data
> --       wevents :: [WEvents]                      
> --       wevents = decode AlignRight bytes         -- parse the weather events data
> --
> --   ...
> --
> --   putStrLn $ "Foggy days: " ++ show (foggyDays wevents)

The expected output is `[2,3,4,5,6,7,11,12,17,18,20,21,22,23,24,26]`.


More data files
----------------

Finally, use this library to process `soccer.dat`, calculating the team with the
smallest absolute difference between the goals \'F\'or and \'A\'gainst an opponent.

> soccer :: String -> String
> soccer = undefined
> 
> soccerProgram :: IO String
> soccerProgram = do
>    bytes <- readFile "soccer.dat"
>    return (soccer bytes)   -- "Aston_Villa"