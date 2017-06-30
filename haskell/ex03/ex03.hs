
{-
Aufgabe 1 [List sugaring] Schreiben Sie die folgenden Ausdrücke so um, dass sie den Listenkonstruktor : ('cons') nicht mehr enthalten.
-}

s1 = 1 : 2 : 3 : [4] -- [1, 2, 3, 4]
s2 = 1 : 2 : [3, 4] -- [1, 2, 3, 4]
s3 = (1 : 2 : []) : (3 : []) : [] -- [[1, 2], [3]]
s4 = (1, 2) : (3, 4) : [(5, 6)] -- [(1, 2), (3, 4), (5, 6)]
s5 = [] : [] -- [[]]
s6 = [] : [] : [] -- [[], []]
s7 = ([] : []) : [] -- [[[]]]
s8 = (([] : []) : []) : [] -- [[[[]]]]
s9 = 'a' : 'b' : [] -- “ab” (or ['a', 'b'] under the hood)

{-
Aufgabe 2 [List desugaring] Schreiben Sie die folgenden Ausdrücke so um, dass sie die eckigen Klammern [ und ] nur noch als Listenkonstruktor [] ('nil') enthalten.
-}

d1 = [1, 2, 3] -- 1 : 2 : 3 : []
d2 = [[1, 2], [], [3, 4]] -- (1 : 2 : []) : [] : (3 : 4 : []) : []
d3 = [[], ["a"], [[]]] -- [] : (('a' : []) : []) : ([] : []) : [] (this would be printed as [[], [“a”], [“”]])

{-
Aufgabe 3 [Pattern Matching] Gegeben seien die folgenden Funktions- und Wertdeklarationen. Geben Sie zu jeder Funktion Ihren Typ an, und werten Sie die Ausdrücke in den Wertdeklarationen aus. Begründen Sie allfällige Fehler bei den Datentypen oder beim Pattern Matching.
-}

f1 (x : y : z) = (x, y, z) -- :: [t] -> (t, t, [t])
f2 [x, y] = (x, y) -- :: [t] -> (t, t)
f3 (x : y : []) = (x, y) -- :: [t] -> (t, t)

a11 = f1 [] -- *** Exception: Non-exhaustive patterns in function f1
a21 = f2 [] -- *** Exception: Non-exhaustive patterns in function f2
a31 = f3 [] -- *** Exception: Non-exhaustive patterns in function f3

a12 = f1 [1] -- *** Exception: Non-exhaustive patterns in function f1
a22 = f2 [1] -- *** Exception: Non-exhaustive patterns in function f2
a32 = f3 [1] -- *** Exception: Non-exhaustive patterns in function f3

a13 = f1 [1, 2] -- (1, 2, []) :: Num t => (t, t, [t])
a23 = f2 [1, 2] -- (1, 2) :: Num t => (t, t)
a33 = f3 [1, 2] -- (1, 2) :: Num t => (t, t)

a14 = f1 [1, 2, 3] -- (1, 2, [3]) :: Num t => (t, t, [t])
a24 = f2 [1, 2, 3] -- *** Exception: Non-exhaustive patterns in function f2
a34 = f3 [1, 2, 3] -- *** Exception: Non-exhaustive patterns in function f3

a15 = f1 (1 : 2 : 3 : []) -- (1, 2, [3]) :: Num t => (t, t, [t])
a25 = f2 (1 : 2 : 3 : []) -- *** Exception: Non-exhaustive patterns in function f2
a35 = f3 (1 : 2 : 3 : []) -- *** Exception: Non-exhaustive patterns in function f3

a16 = f1 ['a', 'b', 'c'] -- ('a', 'b', ['c']) or ('a', 'b', ”c”) :: (Char, Char, [Char])
a17 = f1 [[1], [2, 3], []] -- ([1], [2, 3], [[]]) :: Num t => ([t], [t], [[t]])
a18 = f1 (1 : 2 : 3 : [4, 5]) -- (1, 2, [3, 4, 5]) :: Num t => (t, t, [t])
a19 = f1 [1 .. 100] -- (1, 2, [3..100]) :: Num t => (t, t, [t])

f4 (x, y) = [x, y] -- (t, t) -> [t]
x41 = ([1, 2], [3, 4, 5]) -- :: (Num t1, Num t) => ([t], [t1])
a41 = f4 x41 -- [[1, 2],[3, 4, 5]] :: Num t => [[t]]

g1 :: Num t => [Char] -> t
g1 "dimdi" = 1
g1 ['d', 'o', 'm', 'd', 'o'] = 2
g1 ('d' : 'i' : 'n' : 'g' : []) = 3
g1 ('d' : 'i' : 'n' : 'g' : _) = 4
g1 (x : y) = 5
g1 _ = 6

b11 = g1 "domdo" --  2 :: Num t => t
b12 = g1 "ding" --  3 :: Num t => t
b13 = g1 "dingdimdi" --  4 :: Num t => t
b14 = g1 "dumdu" --  5 :: Num t => t
b15 = g1 "" --  6 :: Num t => t

g2 :: Num t => [Char] -> t
g2 (d : "imdi")
  | d == 'd' || d == 'D' = 1
g2 (z : "umsel")
  | z == 'z' || z == 'Z' = 2
g2 _ = 3

b21 = g2 "dimdi" -- 1 :: Num t => t
b22 = g2 ['D', 'i', 'm', 'd', 'i'] -- 1 :: Num t => t
b23 = g2 ('Z' : 'u' : "msel") -- 2 :: Num t => t
b24 = g2 "dimdiding" -- 3 :: Num t => t

h1 :: [Char] -> Char
h1 ['a', 'b'] = 'a'
h1 ['a', b] = b
h1 (_ : _ : 'm' : _) = 'm'
h1 (a : b) = a

c11 = h1 "ab" -- 'a' :: Char
c12 = h1 "ac" -- 'c' :: Char
c13 = h1 "dimdi" -- 'm' :: Char
c14 = h1  "zumsel" -- 'm' :: Char
c15 = h1 "schnurpsel" -- 's' :: Char

h2 :: [(t, t1)] -> (t, t1)
h2 [(a, b), c] = c
h2 (a : b : c) = a

c21 = h2 [(1, 2), (3, 4)] -- (3, 4) :: (Num t1, Num t) => (t, t1)
c22 = h2 [(1, 2), (3, 4), (5, 6)] -- = (1, 2) :: (Num t1, Num t) => (t, t1)
c23 = h2 [(1, 2)] -- *** Exception: Non-exhaustive patterns in function h2

h3 :: [[Char]] -> [Char]
h3 ((x : y) : z) = y
h3 ([] : _) = "2"
h3 [] = "3"

c31 = h3 ["dimdi"] -- “imdi” :: [Char]
c32 = h3 ["", "dimdi", "domdo"] -- “2” :: [Char]
c33 = h3 [[]] -- “2” :: [Char]
c34 = h3 [] -- “3” :: [Char]
