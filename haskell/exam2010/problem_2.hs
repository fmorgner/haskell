module ExamPreparation.HS2010.Problem2 where

{-
The type below represents a polynomial in the following way:
  Poly => [Terms]
  Term => (Exponent, Coefficient)
-}
type Poly = [(Integer, Integer)]

{-
Representation of 2x^4 - 3x^2 + 5
-}
exaPoly = [(4,2), (2, -3), (0, 5)]

{-
Provide a function to check whether p is a valid representation of a polynomial
subject to the following conditions:
  (1): There is at least one Term (e.g [] is not valid)
  (2): There are no exponents less than 0
  (3): Each terms coefficient is different from 0
  (4): The terms are sorted strongly monotonic descending with respect to their
       exponent
-}

pIsRep :: Poly -> Bool
pIsRep [] = False
pIsRep [x] = isTerm x
  where
    isTerm (a, b) = a >= 0 && b /= 0
pIsRep (x:y:xs) = isTerm x && fst x > fst y && pIsRep (y:xs)
  where
    isTerm (a, b) = a >= 0 && b /= 0

{-
Provide a function to evaluate a given polynomial for a specified value of x
-}
pEval :: Poly -> Integer -> Integer
pEval p x = foldr (\(e, c) -> (+ c * x^e)) 0 p

{-
Provide a function that adds one polynomial to another. Remember the conditions
above.
-}
pSum :: Poly -> Poly -> Poly
pSum p [] = []
pSum [] p = []
pSum pa@(xa@(ea, ca):xas) pb@(xb@(eb, cb):xbs)
  | ea == eb && ca + cb == 0 = pSum xas xbs
  | ea == eb = (ea, ca + cb) : pSum xas xbs
  | ea > eb = xa : pSum xas pb
  | otherwise = xb : pSum pa xbs
