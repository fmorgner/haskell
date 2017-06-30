
{-
Aufgaben
In jeder der folgenden Aufgaben sind einige Deklarationen gegeben. Geben Sie zu jedem deklarierten Wert seinen Typ an, und falls dieser Wert keine Funktion ist, das Ergebnis der Evaluation dieses Wertes. Zur Vereinfachung können Sie annehmen (ausser bei f01 und ihren Varianten), dass alle numerischen Typen Int sind — Sie brauchen also keine Type Constraints aufzuschreiben (aber können dies zur weiteren Übung natürlich trotzdem tun...).
-}

{-
Aufgabe 1 [Lambda expressions]
-}

f01 :: Num a => a -> a
f01 = \x -> 2 * x

f01' = \x -> 2 * x -- :: Num a => a -> a

f01'' () = \x -> 2 * x -- :: Num a => () -> a -> a
f01''' _ = \x -> 2 * x -- :: Num a => t -> a -> a

f02 = \x -> \y -> x + y -- :: Num a => a -> a -> a
f03 = \x y -> x + y -- :: Num a => a -> a -> a
f04 x = \y -> x + y -- :: Num a => a -> a -> a
f05 = \(x, y) -> x + y -- :: Num a => (a, a) -> a
f06 = \[x, y] -> x + y -- :: Num a => [a] -> a

f07 = [\x -> x + 1, \x -> 2 * x, \x -> x ^ 2] -- Num a => [a -> a]
f08 = head f07 5 -- 6 :: Num a => a

f09 = \x -> x -- :: t -> t

f10 = [f09, \x -> x + 1] -- :: Num a => [a -> a]

f11 = \_ -> (\x -> x + 1, \() -> 'a') -- :: Num a => t -> (a -> a, () -> Char)

{-
Aufgabe 2 [Sections]
-}

x ^+^ y = x^2 + y^2 -- :: Num a => a -> a -> a
g01 = (^+^) -- :: Num a => a -> a -> a
g02 = (^+^ 2) -- :: Num a => a -> a
g03 = (3 ^+^) -- :: Num a => a -> a
g04 = (3 ^+^ 2) -- 13 :: Num a => a

g05 x y = 2 * x + 3 * y -- :: Num a => a -> a -> a
g06 = (`g05` 2) -- :: Num a => a -> a
g07 = (2 `g05`) -- :: Num a => a -> a
g08 = g06 3 -- 12 :: Num a => a
g09 = g07 4 -- 16 :: Num a => a

g10 x y z = 2 * x + 3 * y + 4 * z -- :: Num a => a -> a -> a -> a
g14 x = (g10 (x+1)) -- :: Num a => a -> a -> a -> a
g15 = g14 2 3 4 -- 31 :: Num a => a

g16 n = \x -> ([(+), (-), (*)] !! n) x 2 -- :: Num a => Int -> a -> a 
g17 = g16 1 5 -- 2 :: Num a => a (note: index starting at 0)

{-
Aufgabe 3 [List comprehensions]
-}

h01 = [x | x <- [1 .. 5]] -- [1, 2, 3, 4, 5] :: (Num t, Enum t) => [t]
h02 = [2 * x | x <- [1 .. 5]] -- [2, 4, 6, 8, 10] :: (Num t, Enum t) => [t]
h03 = [x - y | x <- [1 .. 3], y <- [1 .. 4]] -- [0, -1, -2, -3, 1, 0, -1, -2, 2, 1, 0, -1] :: (Num t, Enum t) => [t]
h04 = [x - y | y <- [1 .. 3], x <- [1 .. 4]] -- [0, 1, 2, 3, -1, 0, 1, 2, -2, -1, 0, 1] :: (Num t, Enum t) => [t]
h05 = [x + y | x <- [1 .. 3], y <- [1 .. 4], x >= y] -- [2, 3, 4, 4, 5, 6] :: (Ord t, Num t, Enum t) => [t]
h06 = [head x | x <- ["dimdi", "schnurpsel", "zumsel"]] -- ['d', 's', 'z'] or "dsz" :: [Char]
h07 = [x | (x : _) <- ["dimdi", "schnurpsel", "zumsel"]] -- ['d', 's', 'z'] or "dsz" :: [Char]
h08 = [xs | ('s' : xs) <- ["dimdi", "schnurpsel", "zumsel"]] -- ["chnurpsel"] :: [[Char]]
