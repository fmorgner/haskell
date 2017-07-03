-- IML interpreter and VCG
-- Edgar F.A. Lederer, FHNW, 2010

module VCG where

import AbsSyn

vcgen :: Command -> (Assertion, Assertion) -> [Assertion] -> [Assertion]
vcgen cmd (pre, post) accu = vc : accu'
  where
    (pre', accu') = precgen cmd post accu
    vc = DyaBExpr Implies pre pre'

precgen :: Command -> Assertion -> [Assertion] -> (Assertion, [Assertion])
precgen SkipCmd post accu = (weakestPre, accu)
  where
    weakestPre = post
precgen (AssiCmd ident expr) post accu = (weakestPre, accu)
  where
    weakestPre = textSub post (ident, expr)
precgen (CpsCmd cmds) post accu = p (reverse cmds) post accu
  where
    p [] post accu = (post, accu)
    p (cmd : rest) post accu = p rest pre accu'
      where
        (pre, accu') = precgen cmd post accu
precgen (CondCmd guard thenCmd elseCmd) post accu = (pre, accu'')
  where
    (thenPre, accu') = precgen thenCmd post accu
    (elsePre, accu'') = precgen elseCmd post accu'
    thenP = DyaBExpr CondAnd thenPre guard
    elseP = DyaBExpr CondAnd elsePre (NegBExpr guard)
    pre = DyaBExpr CondOr thenP elseP
precgen (WhileCmd guard invar repCmd) post accu = (pre, vc : accu')
  where
    pre = invar
    accu' = vcgen repCmd (DyaBExpr CondAnd invar guard, invar) accu 
    vc = DyaBExpr Implies (DyaBExpr CondAnd invar (NegBExpr guard)) post

vcgProg :: Program -> [Assertion]
vcgProg (Program pre post cmd) = vcgen cmd (pre, post) []

textSub :: Assertion -> (Ident, ArithExpr) -> Assertion
textSub = textSubBExpr

textSubBExpr :: BoolExpr -> (Ident, ArithExpr) -> BoolExpr
textSubBExpr val@(LitBExpr _) _ = val
textSubBExpr (RelBExpr opr expr1 expr2) sub = RelBExpr opr expr1' expr2'
  where
    expr1' = textSubAExpr expr1 sub
    expr2' = textSubAExpr expr2 sub
textSubBExpr (NegBExpr expr) sub = NegBExpr expr'
  where
    expr' = textSubBExpr expr sub
textSubBExpr (DyaBExpr opr expr1 expr2) sub = DyaBExpr opr expr1' expr2'
  where
    expr1' = textSubBExpr expr1 sub
    expr2' = textSubBExpr expr2 sub

textSubAExpr :: ArithExpr -> (Ident, ArithExpr) -> ArithExpr
textSubAExpr val@(LitAExpr _) _ = val
textSubAExpr expr'@(IdAExpr ident') (ident, expr) =
  if ident' == ident then expr else expr'
textSubAExpr (DyaAExpr opr expr1 expr2) sub = DyaAExpr opr expr1' expr2'
  where
    expr1' = textSubAExpr expr1 sub
    expr2' = textSubAExpr expr2 sub
