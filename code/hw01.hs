module Hw1 where

main :: IO ()
main = do
    putStr "What is your name? "
    name <- getLine
    putStr "What is your birth year? "
    year <- getLine 
    putStrLn $ "Hi " ++ name
    putStrLn $ "In 2030, you will be: " ++ show (2030 - read year)