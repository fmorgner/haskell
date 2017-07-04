
{-
Aufgabe 1:
Betrachten Sie eine Funktion eq, die eine Liste von Elementen als Eingabe nimmt, und einen Wahrheitswert als Ausgabe liefert. Der Wahrheitwert ist True, falls alle Elemente der Liste den gleichen Wert haben (oder falls die Liste leer ist), und False sonst.
    (a) Geben Sie den allgemeinstmöglichen Typ von eq an.
    (b) Was ist eq [1]?
    (c) Schreiben Sie die Funktion eq.
-}


-- (a)

eq :: Eq a => [a] -> Bool

-- (b)

{-
eq [1] ist per Definition True.
-}

-- (c)

eq []           = True
eq [_]          = True
eq (x : y : xs) = x == y && eq (y : xs)

{-
Aufgabe 2:
Betrachten Sie eine Funktion neq, die eine Liste von Elementen als Eingabe nimmt, und einen Wahrheitswert als Ausgabe liefert. Der Wahrheitwert ist True, falls alle Elemente der Liste paarweise verschiedene Werte haben (oder falls die Liste leer ist), und False sonst. Beispiel:

    neq [1,2,3,2]

liefert den Wert False, da die Werte der Elemente an den Positionen 1 und 3 gleich sind.
    (a) Geben Sie den allgemeinstmöglichen Typ von neq an. (b) Was ist neq [1]?
    (c) Schreiben Sie die Funktion neq.
-}

-- (a)

neq :: Eq a => [a] -> Bool

-- (b)

{-
neq [1] ist per Definition True
-}

-- (c)

neq [] = True
neq [_] = True
neq (x : xs) = not (elem x xs) && neq xs
