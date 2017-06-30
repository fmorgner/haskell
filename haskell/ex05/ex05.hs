{-
In the following, matrices are lists of lists in row-major ordering. Indexing
into a matrix is 1-based!
-}
type Value = Double
type Matrix = [[Value]]

{-
Check whether a given matrix is actually a matrix, e.g. all rows are of
equal length.
-}
isMat :: Matrix -> Bool
isMat [] = True
isMat (x:xs) = and [length row == lx | row <- xs]
  where lx = length x

{-
Check if a given matrix is square. The first version uses as-patterns, while
the second one relies on 'head'.
-}
isSquareMat :: Matrix -> Bool
isSquareMat [] = True
isSquareMat xs@(x:_) = isMat xs && length xs == length x

isSquareMat' :: Matrix -> Bool
isSquareMat' [] = True
isSquareMat' xs = isMat xs && length xs == length (head xs)

{-
Generate an 'm*n' matrix filled with zeroes. The first implementation is
possibly less efficient than the second one.
-}
zeroMat :: Int -> Int -> Matrix
zeroMat m n = [[0 | _ <- [1..n]] | _ <- [1..m]]

zeroMat' :: Int -> Int -> Matrix
zeroMat' m n = [row | _ <- [1..m]]
  where row = [0 | _ <- [1..n]]

{-
Check if a fiven matrix is a 'zero matrix'.
-}
isZeroMat :: Matrix -> Bool
isZeroMat mat = isMat mat && and [e == 0 | row <- mat, e <- row]

isZeroMat' :: Matrix -> Bool
isZeroMat' mat = isMat mat && and [e == 0 | e <- concat mat]

{-
Generate a 'n*n' unit matrix
-}
unitMatV1 :: Int -> Matrix
unitMatV1 n = [row i | i <- [1..n]]
  where row i = [if x == i then 1 else 0 | x <- [1..n]]

unitMatV2 :: Int -> Matrix
unitMatV2 n = [row i | i <- [1..n]]
  where row i = [0 | _ <- [1..i-1]] ++ 1 : [0 | _ <- [i+1 .. n]]

{-
Check if a given matrix is a unit matrix
-}
isUnitMat :: Matrix -> Bool
isUnitMat mat = equalMat mat $ unitMat (length mat)

{-
Compare two matrices for equality.
-}
equalMat :: Matrix -> Matrix -> Bool
equalMat = (==)

{-
Negate a matrix.
-}
neg :: Matrix -> Matrix
neg mat = [[-e | e <- row] | row <- mat]

{-
Add two matrices
-}
plusMat :: Matrix -> Matrix -> Matrix
mat1 `plusMat` mat2 = [zipWith (+) row1 row2 | (row1, row2) <- zip mat1 mat2]

plusMat' :: Matrix -> Matrix -> Matrix
mat1 `plusMat'` mat2 = [addRows row1 row2 | (row1, row2) <- zip mat1 mat2]
  where addRows row1 row2 = [a + b | (a, b) <- zip row1 row2]

{-
Index into a matrix. Reminder: Indexing matrices is 1-based
-}
readMat :: Matrix -> Int -> Int -> Value
readMat mat i j = mat !! (i - 1) !! (j - 1)

{-
Generate a new matrix with the element at row 'i' column 'j' of the original
matrix replaced by v
-}
updateMat :: Matrix -> Int -> Int -> Value -> Matrix
updateMat mat i j v = [if idx == i then updateRow row else row | (idx, row) <- zip [1..] mat]
  where updateRow row = [if idx == j then v else org | (idx, org) <- zip [1..] row]
