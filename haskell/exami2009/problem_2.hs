
{-
Welche der folgenden (syntaktisch korrekten) Ausdrücke bzw. Funktionen in Haskell sind korrekt getypt, und was ist dann ihr allgemeinster Typ? Hinweis 1: Zur Vereinfachung können Sie dabei annehmen, dass sämtliche numerischen Typen Int sind. Sie brauchen also keine class constraints Num a => anzugeben — aber vielleicht class constraints bezüglich Gleichheit oder Ordnung. Hinweis 2: Bitte schreiben Sie Ihre Löungen direkt hier in das Aufgabenblatt.
-}

a1 :: ([Int], [Char]) -- or: ([Int], String)
a1 = ([1,2,3], "dimdi")

a2 :: Int
a2 = f (1, 2)
    where f (x, y) = x + y

f :: Eq a => a -> a -> [Char] -- or: Eq a => a -> a -> String
f x y
    | x == y = "ok"
    | otherwise = "ko"

a4 :: a -> b -> (b, a, (a, b))
a4 = \x -> \y -> (y, x, (x, y))

a5 :: b -> (b, Char, (Char, b))
a5 = (\x -> \y -> (y, x, (x, y))) 'a'

{-
Wrongly typed!
a6 = [(1, 2), (1, 2, 3), (1, 2, 3, 4)]
-}
