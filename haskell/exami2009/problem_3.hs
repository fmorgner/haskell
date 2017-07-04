
{-
Betrachten Sie eine Funktion delDups, die eine Liste von Elementen als Eingabe nimmt und eine Liste von Elementen als Ausgabe liefert. Die Ausgabeliste enthält dieselben Elemente wie die Eingabeliste, jedoch jedes Element höchstens einmal. Beispiel:

    delDups [3,5,3,4,7,4,4,3,6] = [3,5,4,7,6]

    1. Was ist der allgemeinste Typ von delDups?
    2. Schreiben Sie die Funktion delDups.
-}

delDups :: Eq a => [a] -> [a] -- 1.
delDups [] = [] -- 2.
delDups (x : xs)
    | elem x xs = delDups xs
    | otherwise = x : (delDups xs)
