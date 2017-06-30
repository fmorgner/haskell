
module Scanner where

import AbsSyn
import Data.Char

type Token = (Terminal, Maybe Attribute)

data Terminal
  = ALITERAL
  | BLITERAL
  | IDENT
  | ARITHOPR
  | RELOPR
  | BOOLOPR
  | NOT
  | LPAREN
  | RPAREN
  | BECOMES
  | SEMICOLON
  | SKIP
  | IF
  | THEN
  | ELSE
  | ENDIF
  | WHILE
  | DO
  | ENDWHILE
  | FOR
  | FROM
  | TO
  | ENDFOR
  | COMMA
  | SENTINEL
  deriving (Eq, Show)

data Attribute
  = ALitAttrib Int
  | BLitAttrib Bool
  | IdentAttrib Ident
  | AOprAttrib ArithOperator
  | ROprAttrib RelOperator
  | BOprAttrib BoolOperator
  deriving (Eq, Show)

scanner :: String -> [Token]
scanner cs = s0 (cs, [])

s0 :: (String, [Token]) -> [Token]
s0 ('+'     : cs, accu) = s0 (cs, (ARITHOPR, Just (AOprAttrib Plus)) : accu)
s0 ('-'     : cs, accu) = s0 (cs, (ARITHOPR, Just (AOprAttrib Minus)) : accu)
s0 ('*'     : cs, accu) = s0 (cs, (ARITHOPR, Just (AOprAttrib Times)) : accu)
s0 ('<':'=' : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib LessEq)) : accu)
s0 ('<'     : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib Less)) : accu)
s0 ('>':'=' : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib GreaterEq)) : accu)
s0 ('>'     : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib Greater)) : accu)
s0 ('='     : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib Equal)) : accu)
s0 ('/':'=' : cs, accu) = s0 (cs, (RELOPR, Just (ROprAttrib NotEq)) : accu)
s0 (':':'=' : cs, accu) = s0 (cs, (BECOMES, Nothing) : accu)
s0 ('('     : cs, accu) = s0 (cs, (LPAREN, Nothing) : accu)
s0 (')'     : cs, accu) = s0 (cs, (RPAREN, Nothing) : accu)
s0 (';'     : cs, accu) = s0 (cs, (SEMICOLON, Nothing) : accu)
s0 (','     : cs, accu) = s0 (cs, (COMMA, Nothing) : accu)
s0 (c       : cs, accu)
  | isAlpha c = s0 (s1 (cs, [c], accu))
  | isDigit c = s0 (s2 (cs, digitToInt c, accu))
  | isSpace c = s0 (cs, accu)
  | otherwise = error ("Lexical error: " ++ [c])
s0 ([], accu) = reverse ((SENTINEL, Nothing) : accu)

s1 :: (String, String, [Token]) -> (String, [Token])
s1 (c : cs, accu', accu)
  | isAlphaNum c = s1 (cs, c : accu', accu)
  | otherwise = (c : cs, keywords (reverse accu') : accu)
s1 ([], accu', accu) = ([], keywords (reverse accu') : accu)

s2 :: (String, Int, [Token]) -> (String, [Token])
s2 (c : cs, accu', accu)
  | isDigit c = s2 (cs, 10 * accu' + digitToInt c, accu)
  | otherwise = (c : cs, (ALITERAL, Just (ALitAttrib accu')) : accu)
s2 ([], accu', accu) = ([], (ALITERAL, Just (ALitAttrib accu')) : accu)

keywords :: Ident -> Token
keywords "false" = (BLITERAL, Just (BLitAttrib False))
keywords "true" = (BLITERAL, Just (BLitAttrib True))
keywords "div" = (ARITHOPR, Just (AOprAttrib Div))
keywords "mod" = (ARITHOPR, Just (AOprAttrib Mod))
keywords "cand" = (BOOLOPR, Just (BOprAttrib CondAnd))
keywords "cor" = (BOOLOPR, Just (BOprAttrib CondOr))
keywords "not" = (NOT, Nothing)
keywords "skip" = (SKIP, Nothing)
keywords "if" = (IF, Nothing)
keywords "then" = (THEN, Nothing)
keywords "else" = (ELSE, Nothing)
keywords "endif" = (ENDIF, Nothing)
keywords "while" = (WHILE, Nothing)
keywords "do" = (DO, Nothing)
keywords "endwhile" = (ENDWHILE, Nothing)
keywords "for" = (FOR, Nothing)
keywords "from" = (FROM, Nothing)
keywords "to" = (TO, Nothing)
keywords "endfor" = (ENDFOR, Nothing)
keywords ident = (IDENT, Just (IdentAttrib ident))
