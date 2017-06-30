
{-
Aufgabe 1 [Typen von numerischen Literalen] Bestimmen Sie, ob die folgenden Ausdrücke korrekt getypt sind, und falls ja, geben Sie den entsprechenden Typ an.
-}

2 -- :: Num t => t
2 + 2 -- :: Num a => a
2 :: Int -- :: Int
2 :: Float -- :: Float
(2 + 2) :: Double -- :: Double
2.0 -- :: Fractional t => t
2.0 :: Int -- not well-formed
2 + 2.0 -- :: Fractional a => a
(2 :: Int) + (2 :: Double) -- not well-formed
(2 :: Int) + 2 -- :: Int
(2, 2) -- :: (Num t1, Num t) => (t, t1)
[2, 2] -- :: Num t => [t]
[2, 2.0] -- :: Fractional t => [t]
[2 :: Float, 2 :: Double] -- not well-formed

{-
Aufgabe 2 [Typen von überladenen Funktionen] Gegeben seien die folgenden Funktionsdeklarationen:
-}

f1 x = 2
f2 x = 2 * x
f3 x y z = x == y && y == z
f4 x y z = x < y && y < z
f5 x y z = x == y && y < z
f6 x y = 2 * x < y
f7 x y = min (abs x) (negate y)
f8 x y = [x, y, 2]
f9 x y = x `div` y + x / y

{-
Bestimmen Sie, ob die folgenden Ausdrücke korrekt getypt sind, und falls ja, geben Sie den entsprechenden Typ an.
-}

f1 -- :: Num t1 => t -> t1
f1 'a' -- :: Num t1 => t1
f1 "a" -- :: Num t1 => t1
f1 f1 -- :: Num t1 => t1
f2 -- :: Num a => a -> a
f2 2 -- :: Num a => a
f2 2.0 -- :: Fractional a => a
f2 'a' -- not well-formed
('a', 'b') == ('c', 'd') -- :: Bool
('a', 'b') < ('c', 'd') -- :: Bool
('a', 'b') < ('c', 'd', 'e') -- not well-formed
['a', 'b'] < ['c', 'd', 'e'] -- :: Bool (Note: first, heads compared, then…)
f3 -- :: Eq a => a -> a -> a -> Bool
f3 ('a', 'b') ('a', 'b') ('a', 'b') -- :: Bool
f4 -- :: Ord a => a -> a -> a -> Bool
f4 2 2 -- :: (Ord a, Num a) => a -> Bool
f5 -- :: Ord a => a -> a -> a -> Bool
f5 [2] [] [2, 2] -- :: Bool
f6 -- :: (Ord a, Num a) => a -> a -> Bool
(f6) 2 -- :: (Ord a, Num a) => a -> Bool
f7 -- :: (Ord a, Num a) => a -> a -> a 
f7 (2 :: Int) (2 :: Integer) -- not well-formed
f8 -- :: Num t => t -> t -> [t]
f8 2 2.0 -- :: Fractional t => [t]
f9 -- :: (Fractional a, Integral a) => a -> a -> a
f9 2 2 -- :: (Fractional a, Integral a) => a (*)

{-
(*) <interactive>:3:1: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘it’
      prevents the constraint ‘(Fractional a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance Fractional Double -- Defined in ‘GHC.Float’
        instance Fractional Float -- Defined in ‘GHC.Float’
        ...plus one instance involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘print’, namely ‘it’
      In a stmt of an interactive GHCi command: print it
-}

