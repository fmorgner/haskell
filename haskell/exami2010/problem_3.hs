
{-
Die Komposition (.) von Funktionen ist assoziativ: (f . g) . h = f . (g . h); die Klammern können also weggelassen werden. Schreiben Sie eine Funktion comp in Haskell, die eine Liste von Funktionen fi (1 <= i <= n, n >= 0) als Eingabe nimmt, und die Komposition dieser Funktionen als Ausgabe liefert:

    comp[f1, f2, ..., fn] = f1 . f2 . ··· . fn

    1. Berechnen Sie den Wert des Ausdrucks

        comp [\x -> x - 1, \x -> 2 * x, \x -> x * x] 3

    2. Welche Funktion wird durch den Ausdruck comp [] bezeichnet?
    3. Geben Sie den allgemeinstmöglichen Typ der Funktion comp an.
    4. Implementieren Sie die Funktion comp. Hinweis: Eine besonders elegante und kompakte Lösung erhalten Sie durch Verwendung von Funktionen höherer Ordnung aus dem Standard Prelude.
-}

{-
1.
Das Ergebnis is ((3 * 3) * 2) -1 = 17.
-}

{-
2.
Das entspricht der identity function id.
-}

comp :: [a -> a] -> a -> a -- 3.
comp fs x = foldr id x fs -- 4.
