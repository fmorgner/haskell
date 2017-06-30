
{-
Aufgabe 1 [Typen von Listen und Tupeln] Gegeben sei die folgende Deklaration:
-}

x = 'x'

{-
Bestimmen Sie, ob die folgenden Ausdrücke korrekt getypt sind, und falls ja, geben Sie den entsprechenden Typ an.
-}

x -- :: Char
'x' -- :: Char
"x" -- :: [Char] / :: String
['x'] -- :: [Char] / :: String
[x, 'x'] -- :: [Char] / :: String
[x, x, x, x] -- :: [Char] / :: String
['x', "x"] -- not well-formed
[x, True] -- not well-formed
[x == 'x', True] -- :: [Bool]
[["True"]] -- :: [[[Char]]] / :: [[String]]
[[True, False], True] -- not well-formed
[[True, False], []] -- :: [[Bool]]
('x') -- :: Char (tuple must have at least 2 elements)
(x, 'x') -- :: (Char, Char)
(x, x, x, x) -- :: (Char, Char, Char, Char)
('x', "x") -- :: (Char, [Char])
(x, True) -- :: (Char, Bool)
(x == 'x', True) -- :: (Bool, Bool)
(("True")) -- :: [Char] / :: String (tuple must have at least 2 elements)
((True, False), True) -- :: ((Bool, Bool), Bool)
((True, False), ()) -- :: ((Bool, Bool), ())

{-
Aufgabe 2 [Typen von Listen] Gegeben sei die folgende Deklaration:
-}

a = [True]

{-
Bestimmen Sie, ob die folgenden Ausdrücke korrekt getypt sind, und falls ja, geben Sie den entsprechenden Typ an.
-}

a -- :: [Bool]
a ++ a ++ [True] -- :: [Bool]
a ++ [] -- :: [Bool]
head a -- :: Bool
tail a (*) -- :: [Bool]
head 'x' -- :: not well-formed
head "x" -- Char
tail "x" (*) -- :: [Char] / :: String
"dimdi" !! 2 -- :: Char
"dimdi" ++ "ding" -- :: [Char] / :: String

{-
Aufgabe 3 [Typen von Listen und Tupeln gemischt] Gegeben seien die folgenden Deklarationen:
-}

einkaufsliste =
    [(3, "Widerstand 10kOhm"),
     (5, "Kondensator 0.1microFarad"),
     (2, "Zahnrad 38 Zaehne")]

preisliste =
    [("Zahnrad 38 Zaehne", 1200),
     ("Widerstand 10kOhm", 50),
     ("Widerstand 20kOhm", 50),
     ("Kondensator 0.1microFarad", 50)]

{-
Bestimmen Sie, ob die folgenden Ausdrücke korrekt getypt sind, und falls ja, geben Sie den entsprechenden Typ an.
-}

[(True, 'a'), (False, 'b')] -- :: [(Bool,Char)]
[(True, 'a'), ('b', False)] -- not well-formed
[(True, 'a'), ('a' == 'b', head "a")] -- :: [(Bool,Char)]
([True, 'a' == 'b'], ['a']) -- :: ([Bool],[Char])
('a', 'b', 'c', 'd') -- :: (Char,Char,Char,Char)
('a', ('b', ('c', ('d')))) -- :: (Char,(Char,(Char,Char)))
(('a', 'b'), 'c', 'd') -- :: ((Char,Char),Char,Char)
['a', 'b', 'c', 'd'] -- :: [Char]
einkaufsliste -- :: Num t => [(t,[Char])]
preisliste -- :: Num t => [([Char],t)]
(einkaufsliste, preisliste) -- :: (Num t1, Num t) => ([(t,[Char])],[([Char],t1)])

{-
Aufgabe 4 [Typen von Funktionen und Listen] Gegeben seien die folgenden Deklarationen:
-}

f1 :: Int -> Int
f1 x = x^2 + x + 1

f2 :: Int -> Int
f2 x = 2*x + 1

{-
Bestimmen Sie, ob die folgenden Ausdrücke korrekt getypt sind, und falls ja, geben Sie den entsprechenden Typ an.
-}

f1 -- :: Int -> Int
f1 5 -- :: Int
f1 f2 -- not well-formed
f1 (f2 5) -- :: Int
[f1 5, f2 6, 5, 6] -- :: [Int]
[f1, f2, f1] -- :: [Int -> Int]
[f1 5, f2] -- not well-formed
(f1 5, f2) -- :: (Int, Int -> Int)
([f1, f2, f1] !! 1) 3 -- :: Int
([f1, f2, f1] !! 5) 3 (*) -- :: Int (but when evaluating -> *** Exception: Prelude.!!: index too large)

{-
Aufgabe 5 [Typen von Funktionen mit Currying] Gegeben seien die folgenden Deklarationen:
-}

g1 :: Int -> Int -> Int -> Int
g1 x y z = x^2 + y^2 + z^2

g2 :: Int -> Int -> Int
g2 x y = 2*x + 2*y

{-
Bestimmen Sie, ob die folgenden Ausdrücke korrekt getypt sind, und falls ja, geben Sie den entsprechenden Typ an.
-}

g1 -- :: Int -> Int -> Int -> Int
g1 2 -- :: Int -> Int -> Int
g1 2 3 -- :: Int -> Int
g1 2 3 4 -- :: Int
g1 2 3 4 5 -- not well-formed
(g1, g2) -- :: (Int -> Int -> Int -> Int, Int -> Int -> Int)
(g1 2, g2) -- :: (Int -> Int -> Int, Int -> Int -> Int)
(g1 2 3, g2 4) -- :: (Int -> Int, Int -> Int)
(g1 2 3 4, g2 4 5) -- :: (Int, Int)
[g1, g2] -- not well-formed
[g1 2, g2] -- :: [Int -> Int -> Int]
[g1 2 3, g2 4] -- :: [Int -> Int]
[g1 2 3 4, g2 4 5] -- :: [Int]

{-
Aufgabe 6 [Polymorphe Typen] Gegeben seien die folgenden Deklarationen:
-}

h1 x = (x, x, x)
h2 x = [x, x, x]
h3 x = [(x, x), (x, x)]
h4 x y = (x, y)
h5 x y = [x, y]

{-
Bestimmen Sie, ob die folgenden Ausdrücke korrekt getypt sind, und falls ja, geben Sie den entsprechenden Typ an.
-}

h1 -- :: t -> (t, t, t)
h2 -- :: t -> [t]
h3 -- :: t -> [(t, t)]
h4 -- :: t -> t1 -> (t, t1)
h5 (*) -- :: t -> t -> [t]
h1 'a' -- :: (Char, Char, Char)
h1 True -- :: (Bool, Bool, Bool)
h4 'a' "True" -- :: (Char, Bool)
h5 'a' "True" -- not well-formed
h5 True True -- :: [Bool]
[] -- :: [t]
() -- :: ()
head [] (*) -- :: a (but when evaluating -> *** Exception: Prelude.head: empty list)
head () -- not well-formed
