module ExamPreparation.Problem4 where

data Type  = BoolTy | IntTy deriving (Eq, Show)
data Value = BoolVal Bool | IntVal Int
data Opr   = Plus | Equal | And deriving (Eq)
data Expr = Const Value
          | Var Ident
          | Dyadic Opr Expr Expr
type Ident = String
type Env = Ident -> Type

{- Sub-problem 1:
  Provide the *ABSTRACT* syntax for:

  (a) 1 + true
  (b) x + 3
-}

a = Dyadic Plus (Const (IntVal 1)) (Const (BoolVal True))
b = Dyadic Plus (Var "x") (Const (IntVal 3))

{- Sub-problem 2:
  (a) Provide all valid typings for the following expression:

      ((x + 1) = y) AND (p = q)

  (b) Given a function check, that takes an expression and an environment, and
      returns 'Just Type' if the expression is well-typed or 'Nothing' if it
      isn't, provide the type of 'check'.

  (c) Implement check. Assume that every variable occuring in the expression
      also exists in the environment.
-}

spe1 "x" = IntTy
spe1 "y" = IntTy
spe1 "p" = IntTy
spe1 "q" = IntTy

spe2 "x" = IntTy
spe2 "y" = IntTy
spe2 "p" = BoolTy
spe2 "q" = BoolTy

check :: Expr -> Env -> Maybe Type
check (Const (BoolVal _)) _ = Just BoolTy
check (Const (IntVal _)) _ = Just IntTy
check (Var x) e = Just $ e x
check (Dyadic o l r) e
  | o == Plus && ltype == rtype && ltype == Just IntTy = Just IntTy
  | o == And && ltype == rtype  && ltype == Just BoolTy = Just BoolTy
  | o == Equal && ltype == rtype = ltype
  | otherwise = Nothing
  where
    ltype = check l e
    rtype = check r e
