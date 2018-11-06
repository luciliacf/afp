module Lec1 where      -- comments can begin with two dashes
import Test.HUnit      -- library imports must come at the beginning

hw :: IO ()
hw = putStr "Hello World! \n"

main :: IO ()
main = hw

many :: IO ()
many = do putStr "Hello"     -- each line in the sequence
          putStr " World!"   -- must be an IO action
          putStr "\n"

many' :: IO ()
many' = do
    putStr "Hello"
    putStr " World!"
    putStr "\n"

query :: IO ()
query = do
    putStr "What is your name? "
    n <- getLine
    let y :: String
        y = "Welcome to Advanced FP " ++ n
    putStrLn y

query' :: IO ()
query' = do
    m <- putStr "What is your name? "
    n <- getLine
    putStrLn ("Welcome to Advanced FP " ++ n)
    st <- query2
    return ()

query2 :: IO String
query2 = do
    putStr "What is your name? "
    n <- getLine
    return n

query2' :: IO String
query2' = do
    putStr "What is your name? "
    getLine

t1 :: Test
t1 = 3 ~?= 1 + 2      -- check that the expected value `3`
                      -- matches the result of the computation

numTest :: IO Counts
numTest = runTestTT t1

dotest :: IO ()
dotest = do
    c <- runTestTT (3 ~?= 3)
    print c
