
{-
Exercise 1:
• Produce a list with n identical elements:
        replicate :: Int -> a -> [a]
• Select the nth element of a list:
        (!!) :: [a] -> Int -> a
• Decide if a value is an element of a list:
        elem :: Eq a => a -> [a] -> Bool
-}

and' :: [Bool] -> Bool
and' []       = True
and' (x : xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' []       = []
concat' (x : xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n - 1) a

(!!!) :: [a] -> Int -> a
(x : xs) !!! 0 = x
[x] !!! _      = error "index too large"
(x : xs) !!! n = xs !!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' a []       = False
elem' a (x : xs) = a == x || elem' a xs

{-
Exercise 2:
Define a recursive function:
        merge :: Ord a => [a] -> [a] -> [a]
that merges two sorted lists of values to give a single
sorted list. For example:
        > merge [2, 5, 6] [1, 3, 4]
        [1,2,3,4,5,6]
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys             = ys
merge xs []             = xs
merge (x : xs) (y : ys) = if x < y
                          then x : merge xs (y : ys)
                          else y : merge (x : xs) ys

{-
Exercise 3:
Define a recursive function:
        msort :: Ord a => [a] -> [a]
that implements merge sort, which can be specified by
the following two rules:
        1. Lists of length £ 1 are already sorted.
        2. Other lists can be sorted by sorting the two halves and merging the resulting lists.
-}

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs = merge (msort $ take (length xs `div` 2) xs) (msort $ drop (length xs `div` 2) xs)
