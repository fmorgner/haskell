
{-
Eine m x n-Matrix ganzer Zahlen (m, n >= 1) ist ein rechteckiges Schema der Form

    | a11 ... a1n |
    |  .       .  |
    |  .       .  |
    |  .       .  |
    | am1 ... amn |

wobei die Einträge im Schema a11, ..., amn ganze Zahlen sind; wir führen dafür gleich den Typ

    type Value = Integer

ein. Zur Darstellung von Matrizen in funktionalen Programmiersprachen bieten sich sowohl Funktionen als auch Listen an; im Folgenden betrachten wir beide Möglichkeiten nebeneinander.
-}

{-
Aufgabe 1:
Die Idee bei der Darstellung einer Matrix als Funktion besteht darin, dass wir den Wert eines Eintrages im Schema als Funktion von seinen beiden Indices angeben; beispielsweise ist der Eintrag a23 der Funktionswert einer Funktion a für die beiden Indices 2 und 3. Dies reicht jedoch noch nicht aus: wir müssen zusätzlich noch die Dimension der Matrix angeben, also die Anzahl ihrer Zeilen und Spalten. Damit gelangen wir zu folgenden Typdeklarationen:

    type Dimension = (Int, Int)
    type Function = Int -> Int -> Value
    type MatrixF = (Dimension, Function)

So können wir beispielsweise die 100 x 100-Einheitsmatrix elegant schreiben als:

    matFI :: MatrixF
    matFI = ((100, 100), \i j -> if i == j then 1 else 0)

Geben Sie die Matrix

    | 2 1 4 |
    | 2 2 1 |

als Wert matF1 vom Typ MatrixF an. Hinweise: (1) Mit Vorteil verwenden Sie Pattern Matching. (2) Sie brauchen hier keine Lambda-Abstraktion zu verwenden, wie im Beispiel mit der Einheitsmatrix.
-}

type Value = Integer

type Dimension = (Int, Int)
type Function = Int -> Int -> Value
type MatrixF = (Dimension, Function)

matFI :: MatrixF
matFI = ((3, 3), \i j -> if i == j then 1 else 0)

matF1 :: MatrixF
matF1 = ((2, 3), mat)
    where mat 1 1 = 2
          mat 1 2 = 1
          mat 1 3 = 4
          mat 2 1 = 2
          mat 2 2 = 2
          mat 2 3 = 1

{-
Aufgabe 2:
Eine Möglichkeit der Darstellung einer Matrix mittels Listen besteht darin, jede Zeile der Matrix als Liste von Einträgen aufzufassen, und die gesamte Matrix dann als Liste von Zeilen. (Wir gehen dabei im Folgenden davon aus, dass stets alle Zeilen-Listen einer Matrix die gleiche Länge haben.) Dies führt zu folgenden Typdeklarationen:

    type Vector = [Value]
    type MatrixL = [Vector]

(Statt “Row” für Zeile verwenden wir das allgemeinere "Vector"; das ist für spätere Anwendungen zweckmässig.) Die Matrix aus der vorigen Aufgabe lässt sich damit schreiben als:

    matL1 :: MatrixL
    matL1 =
        [[2, 1, 4],
         [2, 2, 1]]

Geben Sie die 3 x 3-Einheitsmatrix als Wert matLI vom Typ MatrixL an.
-}

type Vector = [Value]
type MatrixL = [Vector]

matLI :: MatrixL
matLI =
    [[1, 0, 0],
     [0, 1, 0],
     [0, 0, 1]]

{-
Aufgabe 3:
Schreiben Sie eine Funktion toL :: MatrixF -> MatrixL, die eine Matrix in Funktionsdarstellung als Eingabe nimmt, und die gleiche Matrix in Listendarstellung als Ausgabe liefert. Hinweis: Mit Vorteil verwenden Sie List Comprehensions.
-}

toL :: MatrixF -> MatrixL
toL ((m, n), gen) = [[gen i j | j <- [1 .. n]] | i <- [1 .. m]]

{-
Schreiben Sie eine Funktion toF :: MatrixL -> MatrixF, die eine Matrix in Listendarstellung als Eingabe nimmt, und die gleiche Matrix in Funktionsdarstellung als Ausgabe liefert. Hinweis: Mit Vorteil verwenden Sie die Funktion (!!) :: [a] -> Int -> a, die das n-te Element einer nicht-leeren Liste zurückliefert.
-}

toF :: MatrixL -> MatrixF
toF mat@(row : rows) = ((m, n), \i j -> (mat !! (i - 1)) !! (j - 1))
    where m = length mat
          n = length row

{-
Die mathematische Schreibweise

    sum(i=a, b) f(i)

bezeichnet die Summe der Funktionswerte f(a) + f(a + 1) + ··· + f(b) — vorausgesetzt natürlich, dass a <= b ist; ist a > b, bezeichnet die Schreibweise einfach den Wert 0.
Schreiben Sie eine Funktion sumf::(Int,Int)->(Int->Value)->Value, die die beiden Grenzen a und b sowie die Funktion f als Eingabe nimmt, und die eben definierte Summe als Ausgabe liefert.
-}

sumf :: (Int, Int) -> (Int -> Value) -> Value
sumf (a, b) f = sum [f x | x <- [a .. b], a <= b]

{-
Die Multiplikation einer m x r Matrix mit einer r x n-Matrix ist wie folgt definiert:

    | a11 ... a1r |   | b11 ... b1n |   | c11 ... c1n |
    |  .       .  |   |  .       .  |   |  .       .  |
    |  .       .  | x |  .       .  | = |  .       .  |
    |  .       .  |   |  .       .  |   |  .       .  |
    | am1 ... amr |   | br1 ... brn |   | cm1 ... cmn |

wobei cij = sum(k=1, r) aik * bkj für 1 <= i <= m, 1 <= j <= n.

Schreiben Sie eine Funktion multF :: MatrixF -> MatrixF -> MatrixF, die zwei Matrizen in Funktionsdarstellung als Eingabe nimmt, und ihr Produkt in Funktionsdarstellung als Ausgabe liefert. Falls die Dimensionen der beiden Matrizen dies gar nicht zulassen, soll die Funktion einen Fehler liefern. Letzteres können Sie leicht erreichen, indem Sie in dem entsprechenden Fall einfach die Funktion error :: String -> a mit einem String als Fehlermeldung aufrufen.
-}

multF :: MatrixF -> MatrixF -> MatrixF
multF ((dA1, dA2), genA) ((dB1, dB2), genB)
    | dA2 /= dB1 = error "incompatible dimensions"
    | otherwise  = ((dA1, dB2), entry)
        where entry i j = sum [(genA i k) * (genB k j) | k <- [1 .. dA2]]

{-
Aufgabe 7:
Die Transponierte einer Matrix entsteht durch Vertauschen von Zeilen und Spalten.
Schreiben Sie eine Funktion transF :: MatrixF -> MatrixF, die eine Matrix in Funktionsdarstellung als Eingabe nimmt, und die Transponierte dieser Matrix in Funktionsdarstellung als Ausgabe liefert.
-}

transF :: MatrixF -> MatrixF
transF ((m, n), gen) = ((n, m), flip gen)

{-
Aufgabe 8:
Schreiben Sie eine Funktion transL :: MatrixL -> MatrixL, die eine Matrix in Listendarstellung als Eingabe nimmt, und die Transponierte dieser Matrix in Listendarstellung als Ausgabe liefert. Arbeiten Sie dabei direkt auf den Listen, nicht mittels transF über Funktionsdarstellungen.
Die Ermittlung der Transponierten kann wie folgt ablaufen: Die zu transponierende Matrix wird in zwei Bestandteile geteilt: ihre erste Spalte, dargestellt als Liste, und die übrigbleibende Matrix, dargestellt wie bisher. Dann wird die Spalte als Zeile aufgefasst, und vor die Transpo- nierte der übrigbleibenden Matrix gehängt. Hinweis: Mit Vorteil verwenden Sie List Comprehensions.
-}

transL :: MatrixL -> MatrixL
transL mat@(row : rows) = [[mat !! i !! j | i <- [0 .. (m - 1)] ] | j <- [0 .. (n - 1)]]
    where m = length mat
          n = length row

{-
Aufgabe 9:
Das Skalarprodukt von zwei n-dimensionalen Vektoren ist wie folgt definiert:

    | a1 |   | b1 |
    | .  |   | .  |
    | .  | x | .  | = sum(i=1, n) ai * bi
    | .  |   | .  |
    | an |   | bn |

Schreiben Sie eine Funktion scalProd :: Vector -> Vector -> Value, die zwei Vektoren als Eingabe nimmt und das Skalarprodukt davon als Ausgabe liefert. Falls die beiden Vektoren unterschiedliche Dimensionen haben, soll die Funktion einen Fehler liefern.
-}

scalProd :: Vector -> Vector -> Value
scalProd v1 v2
    | lv1 /= lv2 = error "imcompatible vector length"
    | otherwise  = sum [(v1 !! i) * (v2 !! i) | i <- [0 .. (lv1 - 1)]]
        where lv1 = length v1
              lv2 = length v2

{-
Aufgabe 10:
Schreiben Sie eine Funktion multL :: MatrixL -> MatrixL -> MatrixL, die zwei Matrizen in Listendarstellung als Eingabe nimmt, und ihr Produkt in Listendarstellung als Ausgabe liefert. Falls die Dimensionen der beiden Matrizen dies gar nicht zulassen, soll die Funktion einen Fehler liefern. Verwenden Sie die Funktionen transL und scalProd. Hinweis: Mit Vorteil verwenden Sie List Comprehensions.
-}

multL :: MatrixL -> MatrixL -> MatrixL
multL matA@(rowA : _) matB
    | lrA /= lmB = error "imcompatible matrix dimensions"
    | otherwise  = [[scalProd (matA !! i) (transB !! j) | j <- [0 .. (lmB - 1)]] | i <- [0 .. (lmA - 1)]]
        where transB = transL matB
              lrA    = length rowA
              lmB    = length matB
              lmA    = length matA

{-
Aufgabe 11:
Praktisch wäre schliesslich noch eine Funktion mult, die zwei Matrizen multiplizieren kann, egal ob sie in Funktionsdarstellung oder Listendarstellung vorliegen. Dazu deklarieren wir einen neuen Datentyp, der beide Darstellungen unter einen Hut bringt:

data Matrix
    = MF MatrixF
    | ML MatrixL

Schreiben Sie eine Funktion mult :: Matrix -> Matrix -> Matrix, die zwei Matrizen in beliebiger Darstellung (jeweils verpackt im Typ Matrix) als Eingabe nimmt und ihr Produkt als Listendarstellung (verpackt im Typ Matrix) als Ausgabe liefert. Die eigentliche Matrixmultiplikation soll in Listendarstellung erfolgen.
-}

data Matrix
    = MF MatrixF
    | ML MatrixL

mult :: Matrix -> Matrix -> Matrix
mult (MF matA) (MF matB) = MF (multF matA matB)
mult (MF matA) (ML matB) = MF (multF matA (toF matB))
mult (ML matA) (MF matB) = ML (multL matA (toL matB))
mult (ML matA) (ML matB) = ML (multL matA matB)
