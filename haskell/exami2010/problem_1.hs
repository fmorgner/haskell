
{-
Im Folgenden sind einige syntaktisch korrekte Ausdrücke in Haskell gegeben. Geben Sie zu jedem korrekt getypten Ausdruck seinen Typ an, und zu jedem nicht korrekt getypten Ausdruck die Begründung, warum er nicht korrekt getypt ist. Hinweis: Zur Vereinfachung können Sie dabei annehmen, dass sämtliche numerischen Typen Int sind. Sie brauchen also keine class constraints Num a => anzugeben — aber vielleicht class constraints bezüglich Gleichheit oder Ordnung. Schreiben Sie Ihre Lösungen direkt hier in das Aufgabenblatt!
-}

a1 :: [[Char]] -- or: [String]
a1 = ["dimdi", ['d', 'i', 'n', 'g']]

{-
Wrongly typed!
a2 = [('a', 1), (2, 'b')]
-}

a3 :: [[t]]
a3 = [] : ([] : [])

a4 :: [[[t]]]
a4 = ([] : []) : []

a5 :: [a -> a]
a5 = [id, id id]

a6 :: Num a => [a -> a]
a6 = [id, \x -> x + 1]

a7 :: (Eq a, Num b) => (a -> a -> Bool, b -> b)
a7 = ((==), \x -> x + 1)

a8 :: Ord a => a -> a -> (Bool, Bool, Bool)
a8 = \x -> \y -> (x > y, x == y, x < y)
