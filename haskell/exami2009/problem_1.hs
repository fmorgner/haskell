
{-
Aufgabe 1:
Betrachten Sie eine Funktion minTup, die ein Paar von Werten als Eingabe
nimmt und den kleineren der beiden Werte als Ausgabe liefert. Beispiel:

    minTup (3, 4) = 3

    (a) Was ist der allgemeinste Typ von minTup?
    (b) Schreiben Sie die Funktion minTup.
-}

minTup :: Ord a => (a, a) -> a -- (a)
minTup (x, y) -- (b)
    | x < y     = x
    | otherwise = y

{-
Aufgabe 2:
Betrachten Sie eine curried Funktion minCur, die zwei Werte als Eingabe nimmt
und den kleineren der beiden Werte als Ausgabe liefert. Beispiel:

    minCur 3 4 = 3

    (a) Was ist der allgemeinste Typ von minCur?
    (b) Schreiben Sie die Funktion minCur.
-}

minCur :: Ord a => a -> a -> a -- (a)
minCur x y = if x < y then x else y -- (b)

{-
Aufgabe 3:
Betrachten Sie eine Funktion höherer Ordnung toCurry, die eine Funktion als Eingabe nimmt und eine curried Funktion als Ausgabe liefert. Die Ausgabe-Funktion hat dabei exakt die gleiche Funktionalität wie die Eingabe-Funktion; nur nimmt die Eingabe-Funktion ein Paar von Werten als Eingabe, während die Ausgabe-Funktion zwei 'getrennte' Werte als Eingabe nimmt. Die Funktion toCurry macht also aus einer Funktion auf einem Paar eine gleichartige curried Funktion auf zwei Werten. Beispielsweise liesse sich minCur aus minTup definieren durch:

    minCur = toCurry minTup

    (a) Was ist der allgemeinste Typ von toCurry?
    (b) Schreiben Sie die Funktion toCurry.
-}

toCurry :: ((a, b) -> c) -> a -> b -> c -- (a)
toCurry f = \x y -> f (x, y) -- (b)

{-
Aufgabe 4:
Betrachten Sie nun die genau umgekehrte Funktion höherer Ordnung toTuple.
Mit dieser Funktion liesse sich beispielsweise minTup aus minCur definieren durch:

    minTup = toTuple minCur

    (a) Was ist der allgemeinste Typ von toTuple?
    (b) Schreiben Sie die Funktion toTuple.
-}

toTuple :: (a -> b -> c) -> ((a, b) -> c) -- (a)
toTuple f = \(x, y) -> f x y -- (b)

{-
Aufgabe 5:
Betrachten Sie zwei Funktionen minList1 und minList2. Beide haben den gleichen Typ und die gleiche Funktionalität, unterscheiden sich aber in ihrer Implementierung. Die Funktionen nehmen eine nicht-leere Liste von Werten als Eingabe, und liefern den kleinsten dieser Werte als Ausgabe.

    - Idee der Implementierung von minList1: Das Minimum einer Liste mit mindestens einem Element ist gleich dem Minimum aus dem ersten Element der Liste und dem Minimum der restlichen Elemente der Liste.
    - Idee der Implementierung von minList2: Das Minimum einer Liste mit mindestens zwei Elementen ist gleich dem Minimum der um das grössere der beiden ersten Elemente der Liste verkürzten Liste.

Ihre Aufgaben:

    (a) Was ist der allgemeinste Typ von minList1 bzw. minList2?
    (b) Warum müssen die Eingabe-Listen mindestens ein Element haben?
    (c) Schreiben Sie die Funktionen minList1 und minList2. Hinweis: Mit Vorteil verwenden Sie dabei die Funktion minCur.
    (d) Welche der beiden Funktionen sind endrekursiv, welche nicht?
-}

minList1 :: Ord a => [a] -> a -- (a)
minList1 [x]      = x -- (c)
minList1 (x : xs) = minCur x (minList1 xs)

minList2 :: Ord a => [a] -> a -- (a)
minList2 [x]          = x -- (c)
minList2 (x : y : xs) = minList2 ((minCur x y) : xs)

{-
(b)
Es gibt keinen sinvollen Rückgabewert für das Minimum aus einer leeren Liste.
-}

{-
(d)
minList1 ist nicht endrekursiv.
minList2 ist endrekursiv.
-}

{-
Aufgabe 6:
Besonders elegant wäre es natürlich, das kleinste Element einer Liste durch Anwendung unserer bekannten Funktionen foldl oder foldr zu bestimmen. Leider geht dies nicht unmittelbar, da die fold-Funktionen in der leeren Liste verankert sind, unsere Listen aber mindestens ein Element haben. Dieses Problem lässt sich aber leicht durch eine Funktion foldleft1 lösen, die wir wie folgt definieren:

    foldleft1 f (x : xs) = foldl f x xs

Ihre Aufgaben:

    (a) Was ist der allgemeinste Typ von foldleft1?
    (b) Schreiben Sie eine Funktion minList3 zur Bestimmung des kleinsten Elementes einer nicht-leeren Liste durch Anwendung von foldleft1 und minCur.
-}

foldleft1 :: (a -> a -> a) -> [a] -> a -- (a)
foldleft1 f (x : xs) = foldl f x xs

minList3 :: Ord a => [a] -> a
minList3 = foldleft1 minCur -- (b)

{-
Aufgabe 7:
Minimum und Maximum von möglicherweise leeren Listen liessen sich leicht definieren, wenn wir die uns zur Verfügung stehenden ganzen Zahlen Int ergänzen würden durch die zwei weitere "Werte" -inf und +inf. In Haskell können wir das leicht durch folgenden neuen Typ IntInf realisieren:

    data IntInf
      = MinusInf
      | Normal Int
      | PlusInf

Dann könnten wir die Ordnungsrelation als Funktion less zwischen Werten vom Typ IntInf definieren durch folgende Tabelle:

    less   | -inf  | m: Int | +inf
    --------------------------------
    -inf   | false | true   | true
    n: Int | false | n < m  | true
    +inf   | false | false  | false

Und schliesslich könnten wir eine Funktion minListInf definieren, die das kleinste Element einer Liste von Werten vom Typ IntInf bestimmt — selbst wenn die Liste leer ist. Beispiel:

    minListInf [Normal 4, Normal 5, Normal 4, PlusInf, Normal 9] = Normal 4

    (a) Welchen Typ hat die Funktion less?
    (b) Schreiben Sie die Funktion less. Verwenden Sie dazu pattern matching. Hinweis: Sie brauchen das pattern matching nicht zu optimieren, z.B. hinsichtlich minimaler Anzahl von Fällen.
    (c) Welchen Typ hat die Funktion minListInf?
    (d) Was ist minListInf []?
    (e) Schreiben Sie die Funktion minListInf. Hinweis: Mit Vorteil schreiben Sie dazu zuerst eine Funktion minInf, die den kleineren von zwei Werten vom Typ IntInf bestimmt.
-}

data IntInf
    = MinusInf
    | Normal Int
    | PlusInf
    deriving Show

less :: IntInf -> IntInf -> Bool -- (a)
less MinusInf MinusInf     = False -- (b)
less MinusInf (Normal _)   = True
less MinusInf PlusInf      = True
less (Normal _) MinusInf   = False
less (Normal x) (Normal y) = x < y
less (Normal _) PlusInf    = True
less PlusInf MinusInf      = False
less PlusInf (Normal _)    = False
less PlusInf PlusInf       = False

minListInf :: [IntInf] -> IntInf -- (c)
minListInf [] = MinusInf
minListInf xs = foldleft1 minInf xs
    where minInf a b = if a `less` b then a else b

{-
(d)
Man könnte MinusInf als möglichen Wert für das Minimum der leeren Liste bedenken.
-}

