
{-
Aufgabe 1:
Betrachten Sie eine Funktion front, die ein Element und eine Liste von Listen von Elementen als Eingabe nimmt und eine Liste von Listen von Elementen als Ausgabe liefert. Die Listen der Ausgabeliste werden erzeugt, indem an jede Liste der Eingabeliste das gegebene Element vorne angefügt wird. Beispiel:

    front 17 [[1, 2, 3], [4, 5]] = [[17, 1, 2, 3], [17, 4, 5]]

    (a) Was ist front "d" [["i","m","d","i"]]?
    (b) Was ist front 'd' ["imdi","omdo"]?
    (c) Was ist der allgemeinstmögliche Typ von front?
    (d) Was ist front 17 [[]], und was ist front 17 []?
    (e) Schreiben Sie die Funktion front.
-}

{-
(a)
> front "d" [["i","m","d","i"]]
[["d","i","m","d","i"]]
-}

{-
(b)
> front 'd' ["imdi","omdo"]
["dimdi","domdo"]
-}

front :: a -> [[a]] -> [[a]] -- (c)

{-
(d)
> front 17 [[]]
[[17]]
> front 17 []
[]
-}

front elem xs = [elem : x | x <- xs]

{-
Aufgabe 2:
Betrachten Sie eine Funktion comb, die eine ganze Zahl n >= 0 (Typ Int) als Eingabe nimmt und eine Liste von Listen von Wahrheitswerten als Ausgabe liefert. Die Ausgabeliste enthält 2^n Listen der Länge n. Durch diese Listen werden alle möglichen Kombinationen von Wahrheitswerten dargestellt, und zwar in der üblichen systematischen Reihenfolge. Beispiel:

  comb 3 =
   [[False,False,False],
    [False,False,True],
    [False,True,False],
    [False,True,True],
    [True,False,False],
    [True,False,True],
    [True,True,False],
    [True,True,True]]

    (a) Was ist der Typ von comb?
    (b) Was ist comb 0?
    (c) Schreiben Sie die Funktion comb. Benutzen Sie dazu die Funktion front als auch die Listenfunktion ++ (append) aus dem Standard Prelude. Hinweis: Überlegen Sie, wie Sie beispielsweise comb 4 aus comb 3 bestimmen können.
-}

comb :: Int -> [[Bool]] -- (a)

{-
(b)
> comb 0
[]
-}

comb 0 = []
comb 1 = [[False], [True]]
comb n = front False (comb (n - 1)) ++ front True (comb (n-1))

{-
Aufgabe 3:
Boolesche Ausdrücke werden nun durch Paare aus einer natürlichen Zahl n (Typ Int) und einer Funktion g dargestellt, die eine Liste der Länge n von Wahrheitwerten auf einen Wahrheitwert abbildet. Beispiel:

    bexpr1 = (2, \[x,y] -> not (x && y) == not x || not y)

    (a) Was ist der Typ von derartig dargestellten booleschen Ausdrücken?
-}

bexpr1 :: (Int, [Bool] -> Bool) -- (a)
bexpr1 = (2, \[x,y] -> not (x && y) == not x || not y)

{-
Im Folgenden bezeichnen wir diesen Typ durch die Abkürzung BExpr. 
-}

{-
Aufgabe 4:
Betrachten Sie eine Funktion all, die eine Liste von Wahrheitswerten als Ein- gabe nimmt und einen Wahrheitswert als Ausgabe liefert. Dieser Wahrheitswert ist true, falls alle Wahrheitswerte in der Liste true sind; sonst ist er false.

    (a) Was ist der Typ von all?
    (b) Was ist all []?
    (c) Schreiben Sie die Funktion all. Verwenden Sie dazu den Operator &&.
-}

type BExpr = (Int, [Bool] -> Bool)

all' :: [Bool] -> Bool -- (a)

{-
(b)
> all' []
True
-}

all' = foldr (&&) True -- (c)

{-
Aufgabe 5:
Betrachten Sie eine Funktion check, die einen booleschen Ausdruck vom Typ BExpr als Eingabe nimmt und einen Wahrheitswert als Ausgabe liefert. Der gelieferte Wahrheitswert ist true, falls der boolesche Ausdruck eine Tautologie ist (das heisst, für alle möglichen Kombinationen von Wahrheitswerten den Wert true liefert); sonst ist der Wahrheitswert false.

    (a) Ist bexpr1 eine Tautologie?
    (b) Was ist der Typ von check?
    (c) Schreiben Sie die Funktion check. Machen Sie dazu Gebrauch von vorher definierten Funktionen.
    (d) Mit der Funktion check lassen sich boolesche Ausdrücke mit beliebig vielen Parametern untersuchen. Wie wächst die Rechenzeit mit der Anzahl der Parameter n?
-}

{-
(a)
Ja, ist eine Tautologie.
-}

check :: BExpr -> Bool -- (b)
check (n, expr) = all' $ [expr val | val <- comb n] -- (c)

{-
(d)
Die Rechenzeit wächst exponentiell, genau wie die Anzahl möglicher Kombinationen der Liste der Wahrheitswerte.
-}
