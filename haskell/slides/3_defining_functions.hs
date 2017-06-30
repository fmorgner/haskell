
{-
Exercise 1:
Consider a function safetail that behaves in the same way as tail, except that safetail maps the empty list to the empty list, whereas tail gives an error in this case. Define safetail using:
        1. a conditional expression.
        2. guarded equations.
        3. pattern matching.
Hint: the library function null :: [a] -> Bool can be used to test if a list is empty.
-}

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
          | null xs   = []
          | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

{-
Exercise 2:
Give three possible definitions for the logical or operator (||) using pattern matching.
-}

(|||) :: Bool -> Bool -> Bool
True ||| _     = True
False ||| True = True
_ ||| _        = False

(||||) :: Bool -> Bool -> Bool
False |||| False = False
_ |||| _         = True

(|||||) :: Bool -> Bool -> Bool
True ||||| False  = True
True ||||| True   = True
False ||||| True  = True
False ||||| False = False

{-
Exercise 3:
Redefine the following version of (&&) using conditionals rather than patterns:
        True && True = True
        _ && _ = False
-}

(&&&) :: Bool -> Bool -> Bool
a &&& b = if a == True then
            if b == True then True else False
              else False

{-
Exercise 4:
Do the same for the following version:
        True && b = b
        False && _ = False
-}

(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a == True then b else
             if a == False then False else b
