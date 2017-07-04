
{-
Aufgabe 1:
Boolesche Ausdrücke sind entweder:

    - eine boolesche Konstante (true, T oder false, F), oder
    - die Negation (not, !) eines booleschen Ausdrucks, oder
    - die Konjunktion (and, &&) von zwei booleschen Ausdrücken, oder
    - die Disjunktion (or, ||) von zwei booleschen Ausdrücken.

Beispiel für einen booleschen Ausdruck:

    (T && F) || !F

    (a) Schreiben Sie einen neuen Datentyp BExpr zur Darstellung boolescher Ausdrücke.
    (b) Stellen Sie unseren Beispielausdruck mit diesem Datentyp dar.
-}

data BExpr -- (a)
    = BConst Bool
    | BNeg BExpr
    | BAnd BExpr BExpr
    | BOr BExpr BExpr
    deriving Show

a = BOr (BAnd (BConst True) (BConst False)) (BNeg (BConst False)) -- (b)

{-
Aufgabe 2:
Betrachten Sie eine Funktion eval, die einen booleschen Ausdruck nach den bekannten Regeln für not, and und or auswertet. Die Auswertung unseres Beispielausdrucks liefert den Wert true.

    (a) Welchen Typ hat die Funktion eval?
    (b) Schreiben Sie die Funktion eval.
-}

eval :: BExpr -> Bool -- (a)
eval (BConst val)       = val -- (b)
eval (BNeg expr)        = not $ eval expr
eval (BAnd expr1 expr2) = (eval expr1) && (eval expr2)
eval (BOr expr1 expr2)  = (eval expr1) || (eval expr2)

{-
Aufgabe 4:
Zwei boolesche Ausdrücke A und B lassen sich auch durch eine Implikation (implies, ->) verknüpfen. Unser Datentyp hat zwar keinen Konstruktor für die Implikation, aber die Implikation lässt sich gemäss folgendem Zusammenhang leicht über die Negation und Disjunktion darstellen:

    A -> B = !A || B

Betrachten Sie eine Funktion makeImpl, die zwei boolesche Ausdrücke A und B als Eingabe nimmt und einen booleschen Ausdruck A -> B als Ausgabe liefert, entsprechend dem eben genannten Zusammenhang.

    (a) Welchen Typ hat die Funktion makeImpl?
    (b) Schreiben Sie die Funktion makeImpl.
-}

makeImpl :: BExpr -> BExpr -> BExpr -- (a)
makeImpl a b = BOr (BNeg a) b -- (b)
