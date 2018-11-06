{-# OPTIONS -fwarn-tabs -fno-warn-type-defaults -fdefer-type-errors #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Kata where

import Data.Char as Char
import Data.List as List
import Data.Maybe as Maybe
import Text.Read  (readMaybe)
import Test.HUnit

data Weather = Weather {
  day :: Int, maxT :: Int, minT :: Int, avg :: Int, dep :: Int, wrt :: String
  } deriving (Eq, Show)


class FromRecord a where
  -- Convert a list of string elements into row
  parseRecord :: [(String,String)] -> Maybe a

-- Access this information from tabulated strings
-- Note that there are more columns in the file, but we ignore the extras
-- and only work with the ones that we care about
instance FromRecord Weather where
  parseRecord (("DY",dy):("MAX",mx):("MIN",mn):("AVG",avg):("DEP",dep):("HDD",_):("CDD",_):("WTR",wrt):_) =
               Just Weather <**> dy <**> mx <**> mn <**> avg <**> dep <**> wrt 
  parseRecord _            = Nothing

-- compare two weather records based on the difference between their
-- minimum and maximum temperatures
cmpDiffTemp :: Weather -> Weather -> Ordering
cmpDiffTemp w1 w2 = compare (diff w1) (diff w2) where
    diff w = abs $ maxT w - minT w

weatherProgram :: IO ()
weatherProgram = do
   bytes <- readFile "jan.dat"
   let wdata :: [Weather]
       wdata = decode AlignRight bytes           -- parse the weather data
       wevents :: [WEvents]
       wevents = decode AlignRight bytes
   let ans = day (minimumBy cmpDiffTemp wdata)   -- compute a stat
--   putStrLn $ "WDATA" ++ show wdata
--   putStrLn $ "WEVENTS" ++ show wevents
   putStrLn $ "The day with minimum temp change was " ++ show ans                      -- day 21
   putStrLn $ "The Day most unseasonable was " ++ show (mostUnseasonable wdata)        -- day 12
   putStrLn $ "The number of days with precipitation was " ++ show (numPrecip wdata)   -- 3 days
   putStrLn $ "Foggy days: " ++ show (foggyDays wevents) -- [2,3,4,5,6,7,11,12,17,18,20,21,22,23,24,26]

-----------------------------------------------------------------

data Alignment  = AlignLeft | AlignRight

testSplitHeader = TestList
  [ splitHeader AlignRight "  Dy MxT   MnT   AvT   HDDay" ~?=
      [("Dy",4),("MxT",4),("MnT",6),("AvT",6),("HDDay",8)],
    splitHeader AlignLeft "       Team            P     W    L" ~?=
      [("",7),("Team",16),("P",6),("W",5),("L",1)]
   ]

-- | Use column headers to determine offsets for each row in a table
splitHeader :: Alignment -> String -> [(String,Int)]
splitHeader _          [] = [] 
splitHeader AlignRight s  = (w, length spaces + length w) : splitHeader AlignRight s''
                              where (spaces,s')  = span isSpace s
                                    (w,s'')      = break isSpace s'
                       
splitHeader AlignLeft s = (w, length spaces + length w) : splitHeader AlignLeft s''
                              where (w,s')       = break isSpace s
                                    (spaces,s'') = span isSpace s'
                                    
splitRow :: [(String,Int)] -> String -> [(String,String)]
splitRow _         [] = []
splitRow []         _ = []
splitRow ((w,n):xs) s = let (sw, s') = splitAt n s
                            ws = words sw
                        in if null ws then [] else (w,head ws): splitRow xs s'  

testSplitRow = splitRow [("",7),("Team",16),("P",6),("L",5)]
                        "    1. Arsenal         38    26   " ~?=
               [("","1."),("Team","Arsenal"),("P","38"),("L","26")]

-- | Decode a data file into a list of data rows
-- any rows in the data file that are unparseable are ignored
decode :: forall a. FromRecord a => Alignment -> String -> [a]
decode alignment str = Maybe.mapMaybe parseRecord (tabulate str) where
 tabulate :: String -> [[(String,String)]]
 tabulate str =
   case lines str of
    []        -> []
    (hd:rows) -> map (splitRow header) rows
        where header = splitHeader alignment hd

class FromField a where
  parseField :: String -> Maybe a

instance FromField Int where
  parseField = readMaybe

infixl 4 <**>
(<**>) :: FromField a => Maybe (a -> b) -> String -> Maybe b
Just f  <**> str = f `fmap` parseField str
Nothing <**> str = Nothing

instance FromField String where
  parseField = Just

data Event = Fog | IntenseFog | Thunder | Ice | Hail
           | FreezingRain | Duststorm | Smoke | BlowingSnow | Tornado
  deriving (Eq, Ord, Enum, Show)

tableEventString = [ ("1",Fog),("2",IntenseFog),("3",Thunder),("4",Ice),("5",Hail),
                     ("6",FreezingRain),("7",Duststorm),("8",Smoke),("9",BlowingSnow),("X",Tornado) ]

instance FromField Event where
   parseField s = lookup s tableEventString

testParseEvent :: Test
testParseEvent = TestList [ parseField "1" ~?= Just Fog,
                            parseField "X" ~?= Just Tornado,
                            parseField "12" ~?= (Nothing :: Maybe Event) ]

newtype Events = Events { getEvents :: [Event] } deriving (Eq,Show)

instance FromField Events where
   parseField str = fmap Events (sequence (map (parseField . (:[])) str))

parseEvents :: String -> Maybe Events
parseEvents str = fmap Events (sequence ((map (parseField . (:[])) str) :: [Maybe Event]))

testParseEvents :: Test
testParseEvents = TestList [ parseField "14" ~?= Just (Events [Fog,Ice]),
                             parseField " " ~?= (Nothing :: Maybe Events) ]

-----------------------------------------------------------------

-- the day with the greatest departure average temperature from normal.
mostUnseasonable :: [Weather] -> Int
mostUnseasonable w = day (maximumBy cmpDep w)
   where cmpDep w1 w2 = compare (dep w1) (dep w2)

-- how many days of the month had some sort of precipitation. 
-- The column marked 'WTR' contains this information, when that column has a 'T', 
-- that indicates a trace amount, which should be included in the result.
numPrecip :: [Weather] -> Int
numPrecip w = length $ (filter (=="T") . map wrt) w

-- the list of all foggy days

data WEvents = WEvents {eday :: Int, wx :: Events} deriving (Eq,Show)

-- Access this information from tabulated strings
-- Note that there are more columns in the file, but we ignore the extras
-- and only work with the ones that we care about
instance FromRecord WEvents where
  parseRecord (("DY",dy):("MAX",_):("MIN",_):("AVG",_):("DEP",_):("HDD",_):("CDD",_):("WTR",_):
               ("SNW",_):("DPTH",_):("SPD",_):("SPD",_):("DIR",_):("MIN",_):("PSBL",_):("S-S",_):("WX",wx):_) =
               Just WEvents <**> dy <**> wx
  parseRecord _  = Nothing

foggyDays :: [WEvents] -> [Int]
foggyDays w =   map eday $ filter isFoggy w
   where isFoggy we = let evs = getEvents (wx we)
                       in elem Fog evs || elem IntenseFog evs

-----------------------------------------------------------------

data Soccer = Soccer {team :: String, for :: Int, agst :: Int} deriving (Eq, Show)

-- Access this information from tabulated strings
-- Note that there are more columns in the file, but we ignore the extras
-- and only work with the ones that we care about
instance FromRecord Soccer where
  parseRecord (("",_):("Team",team):("P",_):("W",_):("L",_):("D",_):("F",for):("A",agst):_) =
               Just Soccer <**> team <**> for <**> agst  
  parseRecord _            = Nothing

cmpDiffS :: Soccer -> Soccer -> Ordering
cmpDiffS s1 s2 = compare (diff s1) (diff s2) where
    diff s = abs $ for s - agst s

soccer :: String -> String
soccer s = let sdata :: [Soccer]
               sdata = decode AlignLeft s   -- parse the soccer data
           in team (minimumBy cmpDiffS sdata)  

soccerProgram :: IO String
soccerProgram = do
   bytes <- readFile "soccer.dat"
   return (soccer bytes)  -- "Aston_Villa"
