
module ParserCombis2 where

infixr 5 +++

-- suffix P indicates a parser
-- suffix C indicates a parser combinator

newtype Parser t a = P { parse :: [t] -> Maybe (a, [t]) }

instance Functor (Parser t) where
  fmap f = (>>= return . f)

instance Applicative (Parser t)

instance Monad (Parser t) where
  return v = P (\toks -> Just (v, toks))
  p >>= f = P (\toks -> case parse p toks of
                          Nothing -> Nothing
                          Just (v, remToks) -> parse (f v) remToks)

failureP :: Parser t a
failureP = P (\_ -> Nothing)

tokenP :: Parser t t
tokenP = P (\toks -> case toks of
                       [] -> Nothing
                       (x:xs) -> Just (x, xs))

satP :: (t -> Bool) -> Parser t t
satP p =
  do
    tok <- tokenP
    if p tok then return tok else failureP

(+++) :: Parser t a -> Parser t a -> Parser t a
p +++ q = P (\toks -> case parse p toks of
                        Nothing -> parse q toks
                        Just (v, remToks) -> Just (v, remToks))

rep0C :: Parser t a -> Parser t [a]
rep0C p = rep1C p +++ return []

rep1C :: Parser t a -> Parser t [a]
rep1C p = do v <- p; vs <- rep0C p; return (v : vs)

sepList1C :: Parser t a -> Parser t b -> ([a] -> c) -> Parser t c
sepList1C elemP sepP f =
  do
    e  <- elemP
    es <- rep0C (do sepP; e <- elemP; return e)
    return (f (e : es))

sepList0C :: Parser t a -> Parser t b -> ([a] -> c) -> Parser t c
sepList0C elemP sepP f = sepList1C elemP sepP f +++ return (f [])

sepIdList1C :: Parser t a -> Parser t b -> Parser t [a]
sepIdList1C elemP sepP =
  do
    e  <- elemP
    es <- rep0C (do sepP; e <- elemP; return e)
    return (e : es)

epsilonP :: Parser t [a]
epsilonP = return []

optC :: Parser t a -> Parser t (Maybe a)
optC elemP = (do e <- elemP; return (Just e)) +++ (do epsilonP; return Nothing)
