-- IML interpreter and VCG
-- Edgar F.A. Lederer, FHNW, 2010

module Interpreter where

import AbsSyn

type Value = Int
type State = Ident -> Value

iniState :: State
iniState = \ident -> error "internal error initial state"

readS :: State -> Ident -> Value
readS s ident = s ident

updateS :: State -> (Ident, Value) -> State
updateS s (ident, val) ident' | ident' == ident = val
                              | otherwise = s ident'

evalAOpr :: ArithOperator -> (Value -> Value -> Value)
evalAOpr Times = (*)
evalAOpr Div = div
evalAOpr Mod = mod
evalAOpr Plus = (+)
evalAOpr Minus = (-)

evalROpr :: RelOperator -> (Value -> Value -> Bool)
evalROpr Less = (<)
evalROpr GreaterEq = (>=)
evalROpr Equal = (==)
evalROpr NotEq = (/=)
evalROpr Greater = (>)
evalROpr LessEq = (>=)

evalBOpr :: BoolOperator -> (Bool -> Bool -> Bool)
evalBOpr CondAnd = (&&)
evalBOpr CondOr = (||)

evalAExpr :: ArithExpr -> State -> Value
evalAExpr (LitAExpr c) _ = c
evalAExpr (IdAExpr ident) state = readS state ident
evalAExpr (DyaAExpr opr expr1 expr2) state =
    operation val1 val2
  where
    operation = evalAOpr opr
    val1 = evalAExpr expr1 state
    val2 = evalAExpr expr2 state

evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (LitBExpr b) _ = b
evalBExpr (RelBExpr opr expr1 expr2) state =
    operation val1 val2
  where
    operation = evalROpr opr
    val1 = evalAExpr expr1 state
    val2 = evalAExpr expr2 state
evalBExpr (NegBExpr expr) state =
  not (evalBExpr expr state)
evalBExpr (DyaBExpr opr expr1 expr2) state =
    operation val1 val2
  where
    operation = evalBOpr opr
    val1 = evalBExpr expr1 state
    val2 = evalBExpr expr2 state

interCmd :: Command -> State -> State
interCmd SkipCmd state = state
interCmd (AssiCmd ident expr) state =
    updateS state (ident, val)
  where
    val = evalAExpr expr state
interCmd (CpsCmd cmds) state =
  foldl (flip interCmd) state cmds
interCmd (CondCmd guard thenCmd elseCmd) state
  | evalBExpr guard state = interCmd thenCmd state
  | otherwise = interCmd elseCmd state
interCmd whileCmd@(WhileCmd guard _ repCmd) state
  | evalBExpr guard state =
      interCmd whileCmd state'
  | otherwise = state
      where
        state' = interCmd repCmd state

interpreter :: Program -> State -> State
interpreter (Program _ _ cmd) = interCmd cmd
