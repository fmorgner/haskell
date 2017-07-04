
{-
Im Folgenden sind einige syntaktisch korrekte Ausdrücke in Haskell gegeben. Geben Sie zu jedem korrekt getypten Ausdruck seinen Typ an, und zu jedem nicht korrekt getypten Ausdruck die Begründung, warum er nicht korrekt getypt ist. Hinweis: Zur Vereinfachung können Sie dabei annehmen, dass sämtliche numerischen Typen Int sind. Sie brauchen also keine class constraints Num a => anzugeben — aber vielleicht class constraints bezüglich Gleichheit oder Ordnung. Schreiben Sie Ihre Lösungen direkt hier in das Aufgabenblatt!
-}

a1 :: [Int]
a1 = 1 : 2 : [3, 4]

a2 :: [[Int]]
a2 = (1 : []) : []

a3 :: [[t]]
a3 = [] : [] : []

a4 :: [[[t]]]
a4 = ([] : []) : []

a5 :: (Eq a) => a -> [Bool]
a5 = \x -> [x == x]

a6 :: (Eq a, Ord b) => (a, b) -> (Bool, Bool)
a6 = \(x, y) -> (x == x, y < y)

a7 :: (Eq a, Ord b) => (a, b) -> [Bool]
a7 = \(x, y) -> [x == x, y < y]

a8 :: (Ord a) => a -> (a -> Bool, a -> Bool)
a8 = \x -> (\y -> x < y, \y -> x > y)
