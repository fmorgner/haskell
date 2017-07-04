
{-
Eine Funktion kann getestet werden, indem man (1) eine Menge von Eingaben für die Funktion auswählt, (2) zu jeder dieser Eingaben die tatsächlich richtige Ausgabe bestimmt, (3) die Funktion auf jede der ausgewählten Eingaben anwendet, und (4) die dabei tatsächlich erhaltenen Ausgaben mit den tatsächlich richtigen vergleicht. Findet man eine Abweichung, so hat man einen Fehler gefunden (der Test war erfolgreich); findet man keine Abweichung, so hat man keinen Fehler gefunden (der Test war erfolglos).
-}

{-
Aufgabe 1:
Betrachten Sie eine Funktion test, die eine Funktion als erste Eingabe nimmt, eine Liste von Paaren als zweite Eingabe, und eine Liste von Paaren als Ausgabe liefert:

    • Die übergebene Funktion ist die zu testende Funktion; vielleicht nennen Sie den Parameter für die Funktion daher sut (subject under test). Wir setzen im Folgenden voraus, dass die zu testende Funktion nur eine einzige Eingabe nimmt (diese Funktion also nicht curried ist).

    • Die übergebene Liste enthält die ausgewählten Testfälle: Paare aus jeweils einer Eingabe und der dazugehörigen tatsächlich richtigen Ausgabe; vielleicht nennen Sie den Parameter für die Liste daher tcs (test cases).

    • Die ausgegebene Liste enthält die fehlgeschlagenen Testfälle: Paare aus jeweils einer Eingabe und einem weiteren Paar; dieses enthält die tatsächlich richtige und die tatsächlich erhaltene Ausgabe (also alle Informationen, um diese Testfälle bequem analysieren zu können).

Beispiel: Die Funktion

    cube :: Int -> Int
    cube x = x^2

soll die dritte Potenz ihrer Eingabe berechnen; wir testen diese Funktion. Erster Satz von Testfällen:

    testCasesCube1 = [(0, 0), (1, 1)]

Hiermit testen wir, ob 03 = 0 und 13 = 1 ist: test cube testCasesCube1
liefert die leere Liste [], der Test war also erfolglos. Wir erweitern daher unseren Satz von Testfällen:

    testCasesCube2 = [(-1, -1), (0, 0), (1, 1), (2, 8)]

Hiermit testen wir zusätzlich, ob (−1)3 = −1 und 23 = 8 ist:

    test cube testCasesCube2

liefert nun die Liste [(-1, (-1, 1)), (2, (8, 4))], also eine Liste mit zwei fehlgeschlagenen Testfällen; dieser Test war also erfolgreich. Im Detail sagt die Liste aus: (−1)3 = −1, abercube (-1) == 1,und23 =8,abercube 2 == 4.

Ihre Aufgaben:
    (a) Geben Sie den allgemeinstmöglichen Typ von test an.
    (b) Schreiben Sie die Funktion test.

Hinweis: Eine List-Comprehension ist hierfür bestens geeignet.
-}

test :: Eq b => (a -> b) -> [(a, b)] -> [(a, (b, b))] -- (a)
test sut tcs = [(inp, (exp, sut inp)) | (inp, exp) <- tcs, exp /= sut inp ] -- (b)

cube :: Int -> Int
cube x = x ^ 2

testCasesCube1 = [(0, 0), (1, 1)] :: [(Int, Int)]
testCasesCube2 = [(-1, -1), (0, 0), (1, 1), (2, 8)] :: [(Int, Int)]

{-
Aufgabe 2:
Wir hatten zu Beginn vorausgesetzt, dass unsere zu testende Funktion nicht curried ist. Was tun wir nun aber, wenn wir eine curried Funktion testen wollen? Bei Beantwortung dieser Frage beschränken wir uns im Folgenden auf curried Funktionen mit genau zwei Eingaben.

Beispiel: Die Funktion

  implies :: Bool -> Bool -> Bool
  implies p q = p || not q

soll die Implikation p ⇒ q ihrer beiden Eingaben p und q ausgeben. Leider können wir die Funktion aber nicht unmittelbar testen, da sie curried ist, unsere Funktion test aber nur uncurried Funktionen akzeptiert. Betrachten Sie nun eine Funktion toTuple, die eine curried Funktion mit zwei Parametern als Eingabe nimmt, und eine gleichwertige uncurried Funktion mit einem geeigneten Paar aus zwei Parametern als Ausgabe liefert. Mit Hilfe dieser Funktion können wir nun unseren Test leicht durchführen:

  test (toTuple implies) testCasesImplies

Dabei wählen wir die Testfälle wie folgt:

  testCasesImplies =
    [((False, False), True),
     ((False, True),  True),
     ((True,  False), False),
     ((True,  True),  True)]

Ihre Aufgaben:
    (a) Geben Sie den allgemeinstmöglichen Typ von toTuple an.
    (b) Schreiben Sie die Funktion toTuple.
    (c) Geben Sie das Ergebnis des soeben durchgeführten Tests von implies exakt an.
-}

toTuple :: (a -> b -> c) -> (a, b) -> c -- (a)
toTuple f = \(x, y) -> f x y -- (b)

implies :: Bool -> Bool -> Bool
implies p q = p || not q

testCasesImplies =
    [((False, False), True),
     ((False, True),  True),
     ((True,  False), False),
     ((True,  True),  True)]

{-
(c)
> test (toTuple implies) testCasesImplies
[((False,True),(True,False)),((True,False),(False,True))]
-}

{-
Aufgabe 3:
Es ist natürlich besonders wichtig, dass Funktionen zum Testen selbst möglichst wenig Fehler enthalten; wir sollten solche Funktionen also ebenfalls (und zwar besonders intensiv) testen. Nun, wir haben in dieser Aufgabenstellung bereits drei Testfälle für unsere Funktion test angegeben, zwei betreffend der Funktion cube, einen betreffend der Funktion implies. Beispielsweise können wir leicht aus den beiden Testfällen betreffend cube einen Test für test erstellen:

  testCasesTest =
    [((cube, testCasesCube1), []),
     ((cube, testCasesCube2), [(-1, (-1, 1)), (2, (8, 4))])]

Ihre Aufgaben:
    (a) Geben Sie den Typ von testCasesTest an.
    (b) Testen Sie die Funktion test, indem Sie die Funktion test geeignet auf sich selbst und den Test testCasesTest anwenden.
    (c) Geben Sie das Ergebnis dieses Tests an.
-}

testCasesTest :: [((Int -> Int, [(Int, Int)]), [(Int ,(Int, Int))])] -- (a)
testCasesTest =
    [
        ((cube, testCasesCube1), []),
        ((cube, testCasesCube2), [(-1, (-1, 1)), (2, (8, 4))])
    ]

res = test (toTuple test) testCasesTest -- (b)

{-
(c)
res = []
> length $ test (toTuple test) testCasesTest
0
Bemerkung: Das kann nicht geprintet werden, weil die Liste in ihren möglichen Tupeln Funktionen enthalten würde und Funktionen können nicht geprintet werden.
-}
