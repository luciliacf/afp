{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP

import Control.Applicative (Alternative(..))
import Control.Monad ()

import State (State)
import qualified State as S

import qualified Parser as P
import qualified ParserCombinators as P

main :: IO ()
main = return ()

-------------------------------------------------------------------------

type Variable = String

newtype Block =
    Block [ Statement ]                 -- { s1; ... sn; }
  deriving (Eq, Show)

data Statement =
    Assign Variable Expression          -- x = e
  | If Expression Block Block           -- if e then s1 else s2
  | While Expression Block              -- while e do s
  deriving (Eq, Show)



data Expression =
    Var Variable                        -- x
  | Val Value                           -- v
  | Op Expression Bop Expression
  deriving (Eq, Show)

data Bop =
    Plus     -- +  :: Int -> Int -> Int
  | Minus    -- -  :: Int -> Int -> Int
  | Times    -- *  :: Int -> Int -> Int
  | Divide   -- /  :: Int -> Int -> Int
  | Gt       -- >  :: Int -> Int -> Bool
  | Ge       -- >= :: Int -> Int -> Bool
  | Lt       -- <  :: Int -> Int -> Bool
  | Le       -- <= :: Int -> Int -> Bool
  deriving (Eq, Show, Enum)

data Value =
    IntVal Int
  | BoolVal Bool
  deriving (Eq, Show)

type Store = Map Variable Value

-------------------------------------------------------------------------
-- Here are some test programs. You can ignore the 80-column limit for this part
-- of the file.

-- test.imp
wTest :: Block
wTest = Block [Assign "x" (Op (Op (Op (Val (IntVal 1)) Plus (Val (IntVal 2))) Minus (Val (IntVal 3))) Plus (Op (Val (IntVal 1)) Plus (Val (IntVal 3)))),
              Assign "y" (Val (IntVal 0)),
              While (Op (Var "x") Gt (Val (IntVal 0)))
                (Block [Assign "y" (Op (Var "y") Plus (Var "x")),
                        Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))])]

-- fact.imp
wFact :: Block
wFact =  Block [ Assign "n" (Val (IntVal 5)),
                Assign "f" (Val (IntVal 1)),
                While (Op (Var "n") Gt (Val (IntVal 0)))
                 (Block [Assign "x" (Var "n"),
                         Assign "z" (Var "f"),
                         While (Op (Var "x") Gt (Val (IntVal 1))) (Block [Assign "f" (Op (Var "z") Plus (Var "f")),
                                                                          Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))]),
                         Assign "n" (Op (Var "n") Minus (Val (IntVal 1)))]) ]

-- abs.imp
wAbs :: Block
wAbs = Block [Assign "x" (Op (Val (IntVal 0)) Minus (Val (IntVal 3))),
             If (Op (Var "x") Lt (Val (IntVal 0)))
                (Block [Assign "x" (Op (Val (IntVal 0)) Minus (Var "x"))]) (Block [])]

-- times.imp
wTimes :: Block
wTimes = Block [Assign "x" (Val (IntVal 10)),
                Assign "y" (Val (IntVal 3)),
                Assign "z" (Val (IntVal 0)),
                While (Op (Var "x") Gt (Val (IntVal 0))) (Block [Assign "z" (Op (Var "z") Plus (Var "y")),
                                                                 Assign "x" (Op (Var "x") Minus (Val (IntVal 1)))])]

-------------------------------------------------------------------------

evalE :: Expression -> State Store Value

evalE (Var _)    = error "evalE: unimplemented"
evalE (Val _)    = error "evalE: unimplemented"
evalE (Op _ _ _) = error "evalE: unimplemented"
 



evalS :: Statement -> State Store ()

evalS (While _ _)      = error "evalS: unimplemented"
evalS (Assign _ _)     = error "evalS: unimplemented"
evalS (If _ _ _)       = error "evalS: unimplemented"

eval :: Block -> State Store ()
eval (Block _)        = error "eval: unimplemented"

exec :: Block -> Store -> Store
exec = error "exec: unimplemented"

run :: Block -> IO ()
run block = do putStrLn "Output Store:"
               print (exec block Map.empty)

---------------------------------------------

class PP a where
  pp :: a -> Doc

instance PP Bop where
  pp Plus   = PP.char '+'
  pp Minus  = PP.char '-'
  pp Times  = PP.char '*'
  pp Divide = PP.char '/'
  pp Gt     = PP.char '>'
  pp Ge     = PP.text ">="
  pp Lt     = PP.char '<'
  pp Le     = PP.text "<="

oneLine :: PP a => a -> String
oneLine = PP.renderStyle (PP.style {PP.mode=PP.OneLineMode}) . pp

indented :: PP a => a -> String
indented = PP.render . pp

instance PP Value where
  pp _ = error "TBD: pretty printing values"

instance PP Expression where
  pp _ = error "TBD: pretty printing expressions"

instance PP Block where
  pp _ = error "TBD: pretty printing blocks"

instance PP Statement where
  pp _ = error "TBD: pretty printing statements"

-- use the C++ precendence level table
level :: Bop -> Int
level Times  = 7
level Divide = 7
level Plus   = 5
level Minus  = 5
level _      = 3    -- comparison operators

------------------------------------------------------------------------

step :: Block -> State Store Block
step _ = undefined

-- | Is this block completely evaluated?
final :: Block -> Bool
final (Block []) = True
final _          = False

-- | Evaluate this block to completion
execStep :: Block -> Store -> Store
execStep = undefined

-- | Evaluate this block for a specified number of steps
boundedStep :: Int -> Block -> State Store Block
boundedStep = undefined

stepper :: Block -> IO ()
stepper b = go b where
  go block  = do
    putBlock block
    putStr "imp> "
    str <- getLine
    case str of
      "x" -> return ()    -- quit the stepper

      _   -> putStrLn "?" >> go block -- unknown command
  putBlock :: Block -> IO ()
  putBlock (Block [])    = putStrLn "done"
  putBlock (Block (s:_)) = putStr   "-->"   >> putStrLn (PP.render (pp s))

------------------------------------------------------------------------

valueP :: P.Parser Value
valueP = intP <|> boolP

intP :: P.Parser Value
intP = error "TBD"

constP :: String -> a -> P.Parser a
constP _ _ = error "TBD"

boolP :: P.Parser Value
boolP = error "TBD"

opP :: P.Parser Bop
opP = error "TBD"

varP :: P.Parser Variable
varP = some P.lower

wsP :: P.Parser a -> P.Parser a
wsP p = error "TBD"

exprP :: P.Parser Expression
exprP = error "TBD"



statementP :: P.Parser Statement
statementP = error "TBD"

toplevelP :: P.Parser Block
toplevelP = error "TBD"
