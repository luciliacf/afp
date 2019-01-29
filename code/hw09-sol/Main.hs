{-# OPTIONS_GHC -fwarn-tabs -fwarn-incomplete-patterns -fdefer-type-errors #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Maybe

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


evalOp :: Value -> Bop -> Value -> Value
evalOp (IntVal x) op (IntVal y)  =
   case op of
      Plus   -> IntVal (x + y)
      Minus  -> IntVal (x - y)
      Times  -> IntVal (x * y)
      Divide -> IntVal (x `div` y)
      Gt     -> BoolVal (x > y)
      Ge     -> BoolVal (x >= y)
      Lt     -> BoolVal (x < y)
      Le     -> BoolVal (x <= y)
evalOp _  _  _  = IntVal 0

evalE :: Expression -> State Store Value
evalE (Var v)       = do {s <- S.get; return (fromMaybe (IntVal 0) (Map.lookup v s)) }
evalE (Val x)       = do {return x}
evalE (Op e1 op e2) = do { v1 <- evalE e1; v2 <- evalE e2; return (evalOp v1 op v2)} 


evalS :: Statement -> State Store ()

evalS sw@(While e st) = do val <- evalE e
                           case val of
                             (BoolVal True) -> do {eval st; evalS sw }
                             _              -> return ()
evalS (Assign v e)    = do s <- S.get
                           val <- evalE e
                           () <- S.put (Map.insert v val s)
                           return ()
evalS (If e st1 st2)  = do val <- evalE e
                           case val of
                             (BoolVal True)  -> eval st1
                             (BoolVal False) -> eval st2
                             _               -> return ()

eval :: Block -> State Store ()
eval (Block [])         = return ()
eval (Block (stm:stms)) = do { evalS stm; eval (Block stms) }

exec :: Block -> Store -> Store
exec block store = S.execState (eval block) store

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
   pp (IntVal n)      = PP.int n 
   pp (BoolVal True)  = PP.text "true"
   pp (BoolVal False) = PP.text "false"

instance PP Expression where
   pp (Var var)      = PP.text var
   pp (Val val)      = pp val                   
   pp (Op e1 bop e2) = let {l1 = levelE e1; l2 = levelE e2; l = level bop;
                            d1 = if l1 < l && l1 /= 0 then PP.parens (pp e1) else pp e1;
                            d2 = if l2 <= l && l2 /= 0 then PP.parens (pp e2) else pp e2 }
                       in d1 PP.<+> pp bop PP.<+> d2                                      

instance PP Block where
  pp (Block [])   = PP.empty 
  pp (Block stms) = PP.sep (map pp stms)

bracedD :: Doc -> Doc
bracedD d = if PP.isEmpty d then PP.lbrace PP.$+$ PP.nest (-PP.lineLength PP.style) PP.rbrace
            else PP.braces (PP.space PP.<> PP.nest 2 d PP.<> PP.space)

instance PP Statement where
  pp (Assign var e) = (PP.text var PP.<+> PP.char '=' PP.<+> pp e) <> PP.semi
  pp (If e bl1 bl2) = PP.text "if" PP.<+> PP.parens (pp e) PP.<+> bracedD (pp bl1) 
                      PP.<+> PP.text "else" PP.<+> bracedD (pp bl2) 
  pp (While e bl)   = PP.text "while" PP.<+> PP.parens (pp e) PP.<+> bracedD (pp bl)  

levelE :: Expression -> Int
levelE (Op _ bop _) = level bop
levelE _            = 0

-- use the C++ precendence level table
level :: Bop -> Int
level Times  = 7
level Divide = 7
level Plus   = 5
level Minus  = 5
level _      = 3    -- comparison operators

------------------------------------------------------------------------

step :: Block -> State Store Block
step (Block (stm@(Assign v e):stms)) = do {evalS stm; return (Block stms) } 
step (Block ((If e bl1 bl2):stms))   = do s <- S.get
                                          let (val,s') = S.runState (evalE e) s
                                          case val of
                                            (BoolVal True)  -> do let (Block stms1) = bl1
                                                                  return (Block (stms1++stms))
                                            (BoolVal False) -> do let (Block stms2) = bl2
                                                                  return (Block (stms2++stms))
                                            _               -> return (Block [])
step (Block (sw@(While e bl):stms)) = do s <- S.get
                                         let (val,s') = S.runState (evalE e) s
                                         case val of
                                            (BoolVal True)  -> do let (Block stms') = bl
                                                                  return (Block (stms'++[sw]++stms))
                                            (BoolVal False) -> return (Block stms)
                                            _               -> return (Block [])
step bl                              = return bl



-- | Is this block completely evaluated?
final :: Block -> Bool
final (Block []) = True
final _          = False

-- | Evaluate this block to completion
execStep :: Block -> Store -> Store
execStep bl s = if final bl then s else sf
                where sf = execStep bl' s'
                      (bl',s') = S.runState (step bl) s 

-- | Evaluate this block for a specified number of steps
boundedStep :: Int -> Block -> State Store Block
boundedStep n bl@(Block []) = do {s <- S.get; return bl}
boundedStep 0 bl            = return bl
boundedStep n bl            = do s <- S.get
                                 let (bl',s') = S.runState (step bl) s
                                 return (S.evalState (boundedStep (n-1) bl') s')   

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

