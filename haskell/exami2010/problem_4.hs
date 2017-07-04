
{-
Bäume lassen sich beispielsweise durch den folgenden Datentyp darstellen:

    data Tree a = Node a [Tree a]

Beispiel:

    tree1 = Node 3 [Node 4 [Node 3 [], Node 2 []], Node 5 []]

    1. Betrachten Sie eine Funktion numNodes, die einen Baum als Eingabe nimmt, und die Anzahl seiner Knoten als Ausgabe liefert. Beispielsweise ist numNodes tree1 gleich 5.
        (a) Geben Sie den allgemeinstmöglichen Typ von numNodes an.
        (b) Implementieren Sie die Funktion numNodes.

    2. Betrachten Sie eine Funktion sumTree, die einen Baum als Eingabe nimmt, und die Summe der Werte seiner Knoten als Ausgabe liefert. Beispielsweise ist sumTree tree1 gleich 17.
        (a) Geben Sie den allgemeinstmöglichen Typ von sumTree an.
        (b) Implementieren Sie die Funktion sumTree.
-}

data Tree a = Node a [Tree a]

tree1 = Node 3 [Node 4 [Node 3 [], Node 2 []], Node 5 []]

-- 1.
numNodes :: Tree a -> Int -- (a)
numNodes (Node _ lst) = 1 + sum [numNodes x | x <- lst] -- (b)

-- 2.
sumTree :: Num a => Tree a -> a -- (a)
sumTree (Node val lst) = val + sum [sumTree x | x <- lst] -- (b)
