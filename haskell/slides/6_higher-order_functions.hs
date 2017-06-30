
{-
Exercise 1:
What are higher-order functions that return functions as results better known as?
-}

{-
Antwort gemäss Dr. Edgar Lederer:

Ich denke, Hutton hat ganz einfach curried functions gemeint. Jede curried function liefert,
sofern man nicht *alle* Parameter zur Verfügung stellt, immer eine Funktion als Resultat.
Und eine Funktion, die eine Funktion als Input und/oder Output hat, ist eine Funktion höherer
Ordnung.
-}

{-
Exercise 2:
Express the comprehension [f x | x <- xs, p x] using the functions map and filter.
-}

comprehension :: (a -> Bool) -> (a -> b) -> [a] -> [b]
comprehension p f xs = map f $ filter p xs

{-
Exercise 3:
Redefine map f and filter p using foldr.
-}

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\ x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\ x xs -> if p x then x : xs else xs) []
