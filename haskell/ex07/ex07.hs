
{-
Develop some functions using recursion over lists. Higher-order functions are not required yet.
-}

{-
delDups deletes duplicates from a list
-}
delDups :: Eq a => [a] -> [a]
delDups [] = []
delDups (x : xs)
  | elem x xs = delDups xs
  | otherwise = x : delDups xs

{-
removeEachSnd removes each second element from a list.
-}
removeEachSnd :: [a] -> [a]
removeEachSnd [x]      = [x]
removeEachSnd (x : xs) = x : removeEachSnd (tail xs)
removeEachSnd _        = []

removeEachSnd' :: [a] -> [a]
removeEachSnd' (x : _ : xs) = x : removeEachSnd' xs
removeEachSnd' xs           = xs

{-
makePairs pairs adjacent elements of a list
-}
makePairs :: [a] -> [(a, a)]
makePairs [x]      = []
makePairs (x : xs) = (x, head xs) : makePairs (tail xs)
makePairs _        = []

makePairs' :: [a] -> [(a, a)]
makePairs' []          = []
makePairs' xs@(_ : ys) = removeEachSnd (zip xs ys)

makePairs'' :: [a] -> [(a, a)]
makePairs'' (x : y : zs) = (x, y) : makePairs'' zs
makePairs'' _            = []

makePairs''' :: [a] -> [(a, a)]
makePairs''' (x : y : xs) = (x, y) : makePairs''' xs
makePairs''' xs           = []

{-
halve divides a list into two lists containing each second element, the first list beginning with the first, the second list beginning with the second.
-}
halve :: [a] -> ([a], [a])
halve list@(_ : xs) = (removeEachSnd list, removeEachSnd xs)
halve _             = ([], [])

halve' :: [a] -> ([a], [a])
halve' xs = h xs [] []
  where
    h (x1 : x2 : xs) accu1 accu2 = h xs (x1 : accu1) (x2 : accu2)
    h [x]            accu1 accu2 = h [] (x : accu1) accu2
    h []             accu1 accu2 = (reverse accu1, reverse accu2)

halve'' :: [a] -> ([a], [a])
halve'' []          = ([], [])
halve'' xs@(_ : ys) = (removeEachSnd xs, removeEachSnd ys)

{-
divideList divides a list into chunks of length n each, except of the last chunk, which might be shorter.
Precondition: n > 0
Theorem: For all n > 0 and all xs: concat (divideList n xs) == xs
-}
divideList :: Int -> [a] -> [[a]]
divideList _ [] = []
divideList n xs = take n xs : divideList n (drop n xs)
