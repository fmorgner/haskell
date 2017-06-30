
{-
Exercise 1:
Try out slides 2-7 and 13-16 using GHCi.
-}

-- No solution needed.

{-
Exercise 2:
Fix the syntax errors in the program below, and test your solution using GHCi.
        N = a ’div’ length xs
            where
               a = 10
              xs = [1,2,3,4,5]
-}

n = a `div` length xs
      where
            a = 10
            xs = [1,2,3,4,5]

{-
Exercise 3:
Show how the library function last that selects the last element of a list can be defined using the functions introduced in this lecture.
-}

last' :: [a] -> a
last' xs = head $ reverse xs

{-
Exercise 4:
Can you think of another possible definition?
-}

last'' :: [a] -> a
last'' xs = drop (length xs - 1) xs !! 0

{-
Exercise 5:
Similarly, show how the library function init that removes the last element from a list can be defined in two different ways.
-}

init' :: [a] -> [a]
init' xs = take (length xs - 1) xs

init'' :: [a] -> [a]
init'' xs = reverse $ drop 1 $ reverse xs
