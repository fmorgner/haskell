
module Parser2 where

import Scanner
import ParserCombis2
import AbsSyn

type ParserT = Parser Token

tokP :: Terminal -> ParserT Token
tokP term = satP (\(term', _) -> term' == term)

tP :: Terminal -> ParserT ()
tP term = do tokP term; return ()

aLitP :: ParserT Int
aLitP = do (ALITERAL, Just (ALitAttrib val)) <- tokP ALITERAL; return val

bLitP :: ParserT Bool
bLitP = do (BLITERAL, Just (BLitAttrib val)) <- tokP BLITERAL; return val

identP :: ParserT Ident
identP = do (IDENT, Just (IdentAttrib ident)) <- tokP IDENT; return ident

arithOprP :: ParserT ArithOperator
arithOprP = do (ARITHOPR, Just (AOprAttrib opr)) <- tokP ARITHOPR; return opr

relOprP :: ParserT RelOperator
relOprP = do (RELOPR, Just (ROprAttrib opr)) <- tokP RELOPR; return opr

boolOprP :: ParserT BoolOperator
boolOprP = do (BOOLOPR, Just (BOprAttrib opr)) <- tokP BOOLOPR; return opr

arithExprP :: ParserT ArithExpr
arithExprP = litAExprP +++ idAExprP +++ dyaAExprP

litAExprP = do val <- aLitP; return (LitAExpr val)

idAExprP = do ident <- identP; return (IdAExpr ident)

dyaAExprP :: ParserT ArithExpr
dyaAExprP = dyaAExprP_V04

dyaAExprP1_V04 :: ParserT (ArithOperator, ArithExpr)
dyaAExprP1_V04 =
  do tP LPAREN
     aExpr1 <- arithExprP
     aOpr   <- arithOprP
     return (aOpr, aExpr1)

funDyaAExprP2_V04 :: (ArithOperator, ArithExpr) -> ParserT ArithExpr
funDyaAExprP2_V04 (aOpr, aExpr1) =
  do aExpr2 <- arithExprP
     tP RPAREN
     return (DyaAExpr aOpr aExpr1 aExpr2)

dyaAExprP_V04 :: ParserT ArithExpr
dyaAExprP_V04 =
  dyaAExprP1_V04 >>= funDyaAExprP2_V04

boolExprP :: ParserT BoolExpr
boolExprP = litBExprP +++ relBExprP +++ negBExprP +++ dyaBExprP

litBExprP = do val <- bLitP; return (LitBExpr val)

relBExprP =
  do tP LPAREN
     aExpr1 <- arithExprP
     rOpr   <- relOprP
     aExpr2 <- arithExprP
     tP RPAREN
     return (RelBExpr rOpr aExpr1 aExpr2)

negBExprP =
  do tP NOT
     bExpr <- boolExprP
     return (NegBExpr bExpr)

dyaBExprP =
  do tP LPAREN
     bExpr1 <- boolExprP
     bOpr   <- boolOprP
     bExpr2 <- boolExprP
     tP RPAREN
     return (DyaBExpr bOpr bExpr1 bExpr2)

commandP :: ParserT Command
commandP = skipCmdP +++ assiCmdP +++ multiAssiCmdP +++ condCmdP +++ whileCmdP +++ forCmdP

skipCmdP = do (tP SKIP); return SkipCmd

assiCmdP =
  do ident <- identP
     tP BECOMES
     aExpr <- arithExprP
     return (AssiCmd ident aExpr)

multiAssiCmdP =
  do lhs <- sepIdList1C identP (tP COMMA)
     tP BECOMES
     rhs <- sepIdList1C arithExprP (tP COMMA)
     return (MultiAssiCmd lhs rhs)

condCmdP =
  do tP IF
     bExpr <- boolExprP
     tP THEN
     cmd1  <- cpsCmdP
     tP ELSE
     cmd2  <- cpsCmdP
     tP ENDIF
     return (CondCmd bExpr cmd1 cmd2)

whileCmdP =
  do tP WHILE
     bExpr <- boolExprP
     tP DO
     cmd   <- cpsCmdP
     tP ENDWHILE
     return (WhileCmd bExpr cmd)

forCmdP =
  do tP FOR
     ident  <- identP
     tP FROM
     aExpr1 <- arithExprP
     tP TO
     aExpr2 <- arithExprP
     tP DO
     cmd    <- cpsCmdP
     tP ENDFOR
     return (ForCmd ident aExpr1 aExpr2 cmd)

cpsCmdP :: ParserT Command
cpsCmdP = sepList1C commandP (tP SEMICOLON) CpsCmd

programP :: ParserT Command
programP =
  do cmd <- cpsCmdP
     tP SENTINEL
     return cmd

parser :: [Token] -> Command
parser toks =
  case parse programP toks of
    Just (cmd, []) -> cmd
    Just (a, _)    -> error "internal error"
    n              -> error ("syntax error: " ++ show n)
