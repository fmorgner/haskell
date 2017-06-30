module AbsSyn where

type Ident = String

data ArithOperator
  = Times
  | Div
  | Mod
  | Plus
  | Minus
  deriving (Eq, Show)

data RelOperator
  = Less
  | GreaterEq
  | Equal
  | NotEq
  | Greater
  | LessEq
  deriving (Eq, Show)

data BoolOperator
  = CondAnd
  | CondOr
  deriving (Eq, Show)

data ArithExpr
  = LitAExpr Int
  | IdAExpr Ident
  | DyaAExpr ArithOperator ArithExpr ArithExpr
  deriving (Eq, Show)

data BoolExpr
  = LitBExpr Bool
  | RelBExpr RelOperator ArithExpr ArithExpr
  | NegBExpr BoolExpr
  | DyaBExpr BoolOperator BoolExpr BoolExpr
  deriving (Eq, Show)

data Command
  = SkipCmd
  | AssiCmd Ident ArithExpr
  | MultiAssiCmd [Ident] [ArithExpr]
  | CpsCmd [Command]
  | CondCmd BoolExpr Command Command
  | WhileCmd BoolExpr Command
  | ForCmd Ident ArithExpr ArithExpr Command
  deriving (Eq, Show)
