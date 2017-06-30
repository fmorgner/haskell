
import System.IO
import Control.Applicative
import Data.Char

{-
Exercise 1:
Why does factorising the expression grammar make the resulting parser more efficient?
-}

{-
Without left-factorising, the resulting parser would backtrack excessively and
take exponential time in the size of the expression. For example, a number would
be parsed four times before being recognised as an expression.
-}

{-
Exercise 2:
Extend the expression parser to allow the use of subtraction and division, based upon the following extensions to the grammar:

   expr → term ('+' expr | '-' expr | ε)
   term → factor ('*' term | '/' term | ε)

-}

newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   pure v = P (\inp -> [(v,inp)])
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\ inp -> [])

returnp :: a -> Parser a
returnp v = P (\ inp -> [(v,inp)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\ inp -> case parse p inp of
               [] -> parse q inp
               [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then returnp x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (x ==)

expr :: Parser Int
expr = do t <- term
          do char '+'
             e <- expr
             returnp (t + e)
             +++ do char '-'
                    e <- expr
                    returnp (t - e)
                    +++ returnp t

term :: Parser Int
term = do f <- factor
          do char '*'
             t <- term
             returnp (f * t)
             +++ do char '/'
                    t <- term
                    returnp (f `div` t)
                    +++ returnp f

factor :: Parser Int
factor = do d <- digit
            returnp (digitToInt d)
            +++ do char '('
                   e <- expr
                   char ')'
                   returnp e

eval :: String -> Int
eval xs = fst (head (parse expr xs))
