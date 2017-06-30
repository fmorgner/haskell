module ExamPreparation.Problem3 where

{-
Imagine a function 'repfor' that takes two parameters 'n' and 'f', where 'n' is
the number of times it will apply 'f'. For example:
  thrice f = repfor 3 f

(a) Provide the most generic type for 'repfor'
(2) Implement 'repfor'
-}
repfor :: (Ord a, Num a) => a -> (b -> b) -> b -> b
repfor n f
  | n > 0 = f . repfor (n - 1) f
  | otherwise = id

{-
Alternative implementation with DIFFERENT(!!!) type
-}
repfor' :: Int -> (b -> b) -> b -> b
repfor' n f x = foldr id x (replicate n f)

{-
Demonstrate that 'repfor 3 = thrice':

repfor 3 f =>
  -> (f . repfor 2 f)
  -> (f . f . repfor 1 f)
  -> (f . f . f . repfor 0 f)
  -> (f . f . f . id)
-}

{-
Implement the summation of all integral numbers in the range 0..n using repfor.
-}
sumOn n = snd $ repfor n (\(i, s) -> (i + 1, i + s)) (0, 0)
