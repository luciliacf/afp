{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Quiz
    ( quiz
    ) where

import Debug.Trace
import System.IO.Unsafe
import Data.IORef

-- | A datatype with a "strict" field and a lazy field. We use
-- a "BangPattern" to annotate that `strict` should be "strict." Why the
-- quotes? Well, because it's not really as strict as you might expect!
data Foo = Foo { strict :: !Int, lazy :: Int }

-- | A global variable. Yes, Haskell has global variables, you just have to
-- be careful with them.
lineNumber :: IORef Int
lineNumber = unsafePerformIO (newIORef 0)
{-# NOINLINE lineNumber #-}

-- |  This action is used to keep track of where we are in the quiz.
logLine :: IO ()
logLine = do
    i <- atomicModifyIORef' lineNumber (\i -> (i + 1, i + 1))
    putStrLn $ "Log line: " ++ show i

-- | The 'trace' functions from Debug.Trace will print a message to the
-- console whenever the expression is evaluated. It will only print it
-- once, and after that, the value will be cached.
quiz :: IO ()
quiz = do
    logLine
    let a = trace "evaluating a" $ 1 + 2
    logLine
    let !b = trace "evaluating b" $ 2 + 4
    logLine
    let foo = Foo
            { strict = trace "evaluating strict" $ 1 + 2 + 3 + 4
            , lazy = trace "evaluating lazy" $ 9 + 12
            }
    logLine
    print a
    logLine
    print a -- this line is not a typo
    logLine
    case foo of
        Foo { strict, lazy } -> do
            logLine
            print lazy
