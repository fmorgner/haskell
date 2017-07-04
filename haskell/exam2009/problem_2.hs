
{-
Im Folgenden soll unser Interpreter aus dem Unterricht um eine simultane Zuweisung ergänzt werden. Eine simultane Zuweisung hat in konkreter Syntax oft die folgende Form:

    id1, id2, ..., idn := expr1, expr2, ..., exprn

wobei idi paarweise verschiedene Identifier sind, und expri beliebige arithmetische Ausdrücke (1 <= i <= n); schliesslich ist n >= 1. Die Bedeutung ist die folgende: Die arithmetischen Ausdrücke expr1, ..., exprn werden alle in demselben aktuellen Zustand ausgewertet; dies liefert die Werte v1, ..., vn (expr1 liefert v1, etc.). Diese Werte werden dann an die Identifier id1, ..., idn zugewiesen (v1 an id1, etc.). Beispielsweise bewirkt die berühmte simultane Zuweisung:

    x, y := y, x

ein elegantes Vertauschen der Werte von x und y.
-}

{-
Aufgabe 1:
Begründen Sie, warum keine zwei Identifier auf der linken Seite der simultanen Zuweisung gleich sein dürfen.
-}

{-
Es ist dann nicht klar, welche Zuweisung die letzte sein soll und deshalb ist der resultierende State ambiguous.
-}

{-
Aufgabe 2:
Auf der Ebene der abstrakten Syntax beschreiben wir die simultane Zuweisung im Folgenden durch ein Paar aus einer Liste von Identifiern und einer Liste von arithmetischen Ausdrücken (und nicht durch eine Liste von Paaren aus Identifiern und Ausdrücken). Ergänzen Sie den Datentyp Command unseres Interpreters

     data Command
       = SkipCmd
       | AssiCmd (Ident, ArithExpr)
       | ???
       | CpsCmd [Command]
       | CondCmd (BoolExpr, Command, Command)
       | WhileCmd (BoolExpr, Command)

um einen Konstruktor für die simultane Zuweisung (die normale Zuweisung sollte man nun eigentlich weglassen).
-}

data Command
    = SkipCmd
    | AssiCmd (Ident, ArithExpr)
    | MultiAssiCmd ([Ident], [ArithExpr])
    | CpsCmd [Command]
    | CondCmd (BoolExpr, Command, Command)
    | WhileCmd (BoolExpr, Command)

{-
Aufgabe 3:
Betrachten Sie eine Funktion exDups, die überprüft, ob eine gegebene Liste Duplikate enthält: falls ja, liefert sie True zurück, ansonsten False.

    - Was ist der allgemeinste Typ von exDups?
    - Schreiben Sie die Funktion exDups. Hinweis: Mit Vorteil verwenden Sie dazu die Funktion elem, die entscheidet, ob ein Wert Element einer Liste ist.
-}

exDups :: Eq a => [a] -> Bool
exDups []       = False
exDups (x : xs) = elem x xs || exDups xs

{-
Aufgabe 4:
Schreiben Sie eine Funktion checkCmd :: Command -> Bool, die entscheidet, ob eine gegebene simultane Zuweisung alle syntaktischen Bedingungen erfüllt, die an sie gestellt werden; diese Bedingungen sind zu Beginn dieser Aufgabe genau beschrieben. Die Funktion braucht hier nur für die simultane Zuweisung definiert zu sein, nicht auch für die anderen Arten von Anweisungen.
-}

checkCmd :: Command -> Bool
checkCmd (MultiAssiCmd (idents, exprs)) = li >= 1 && li == le && not (exDups idents)
    where li = length idents
          le = length exprs

{-
Aufgabe 5:
Ergänzen Sie die Funktion interCmd :: Command -> State -> State, unseren eigentlichen Interpreter, so dass er auch die simultane Zuweisung interpretieren kann. (Schreiben Sie nur diesen einen neuen Fall auf, wiederholen Sie nicht die bekannten Fälle.) Sie benötigen dazu unsere bereits bekannten Funktionen

    evalAExpr :: ArithExpr -> State -> Value
    updateS :: State -> (Ident, Value) -> State

Hinweis: Mit Vorteil verwenden Sie ausserdem die Funktionen map, zip und foldl.
-}

type State = Ident -> Value

interCmd :: Command -> State -> State
interCmd multAs@(MultiAssiCmd (idents, exprs)) state
    | not (checkCmd multAs) = error "Command not well-formed"
    | otherwise             = foldl updateS state assis
        where vals  = map (flip evalAExpr state) exprs
              assis = zip idents vals
