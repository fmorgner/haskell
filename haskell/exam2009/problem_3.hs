
{-
Gegeben sei die folgende Datentypdeklaration:

    data T = A | B T | C (T, Int, T)

Welche der folgenden (syntaktisch korrekten) Ausdrücke bzw. Funktionen in Haskell sind korrekt getypt, und was ist dann ihr allgemeinster Typ? Hinweis 1: Zur Vereinfachung können Sie dabei annehmen, dass sämtliche numerischen Typen Int sind. Sie brauchen also keine class constraints Num a => anzugeben — aber vielleicht class constraints bezüglich Gleichheit oder Ordnung. Hinweis 2: Bitte schreiben Sie Ihre Lösungen direkt hier in das Aufgabenblatt.
-}

data T = A | B T | C (T, Int, T)

a1 :: [T]
a1 = [B A, B (B A), C (A, 17, B A)]

a2 :: (T, Int, T) -> T
a2 = C

a3 :: (Int, Int) -> (Int, Bool)
a3 = \(x, y) -> (x + y, x == y)

a4 :: [Int -> Int -> Int]
a4 = [\x y -> x + y, \x y -> x * y]

a5 :: ([t], [Int])
a5 = ([], [1])
