module ExamPreparation.Problem3 where

repfor :: (Ord a, Num a) => a -> (b -> b) -> b -> b
repfor n f
  | n > 0 = f . repfor (n - 1) f
  | otherwise = id

-- repfor' :: Int -> (b -> b) -> b -> b
repfor' n f x = foldr id x (replicate n f)

{-

repfor 3 f

-> (f . repfor 2 f)
-> (f . f . repfor 1 f)
-> (f . f . f . repfor 0 f)
-> (f . f . f . id)

-}

sumOn n = snd $ repfor n (\(i, s) -> (i + 1, i + s)) (0, 0)
