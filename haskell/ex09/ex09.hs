
{-
Exercise (Higher-Order Functions)
-}

import Prelude hiding (flip, curry, uncurry)

{-
flip f takes its (first) two arguments in the reverse order of f
-}
flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

{-
curry converts a function on pairs to a curried function
-}
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f a b = f (a, b)

{-
uncurry converts a curried function to a function on pairs
-}
uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (a, b) = f a b

{-
implement reverse using foldr and (++)
-}
reverseFr :: [a] -> [a]
reverseFr = foldr (\a b -> b ++ [a]) []

{-
implement reverse using foldl, (:), and flip
-}
reverseFl :: [a] -> [a]
reverseFl = foldl (flip (:)) []

{-
revAppend prepends the first list in reverse order before the second list.
implement revAppend using foldl and flip
-}
revAppend :: [a] -> [a] -> [a]
revAppend = flip $ foldl $ flip (:)

mapFr :: (a -> b) -> [a] -> [b]
mapFr f = foldr ((:) . f) []

mapFl :: (a -> b) -> [a] -> [b]
mapFl f = foldl (flip ((:) . f)) [] . reverseFl

filterFr :: (a -> Bool) -> [a] -> [a]
filterFr f = foldr (\a b -> if f a then a : b else b) []

filterFl :: (a -> Bool) -> [a] -> [a]
filterFl f = foldl (\b a -> if f a then b ++ [a] else b) []

lengthFr :: [a] -> Int
lengthFr = foldr (\_ b -> b + 1) 0

lengthFl :: [a] -> Int
lengthFl = foldl (\b _ -> b + 1) 0

appendFr :: [a] -> [a] -> [a]
appendFr = foldr (:)

appendFl :: [a] -> [a] -> [a]
appendFl = foldl (\b a -> b++[a])

concat2Fr :: [a] -> [a] -> [a]
concat2Fr = flip (foldr (:))

{-
strange version, since it uses the library version (++) of exactly what is to be implemented
-}
concat2Fl :: [a] -> [a] -> [a]
concat2Fl = foldl (\accu x -> accu ++ [x])
