module ExamPreparation.Problem1 where

{-
Given the ADT defined below, provide typings for the following expressions.
If the expression is not well-typed, provide the reason.
-}
data D = C0 | C1 D | C2 D D

C2 (C1 C0)    -- D -> D
[[], [['a']]] -- [[[Char]]]
[['a'], [[]]] -- NOT WELL TYPED (replace ' with ")
((<), (==))   -- (Eq a, Ord a) => (a -> a -> Bool, a -> a -> Bool)
[(<), (==)]   -- (Eq a, Ord a) => [a -> a -> Bool]
