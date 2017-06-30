
{-
Exercise 1:
Using recursion and the function add, define a function that multiplies two natural numbers.
-}

data Nat = Zero | Succ Nat deriving Show

addNat :: Nat -> Nat -> Nat
addNat Zero n     = n
addNat (Succ m) n = Succ (addNat m n)

multNat :: Nat -> Nat -> Nat
multNat Zero n     = Zero
multNat (Succ m) n = addNat n (multNat m n)

{-
Exercise 2:
Define a suitable function folde for expressions, and give a few examples of its use.
-}

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
folde f g h (Val n)   = f n
folde f g h (Add x y) = g (folde f g h x) (folde f g h y)
folde f g h (Mul x y) = h (folde f g h x) (folde f g h y)

evale :: Expr -> Int
evale = folde id (+) (*)

sizee:: Expr -> Int
sizee = folde (const 1) (+) (+)

{-
Exercise 3:
A binary tree is complete if the two sub-trees of every node are of equal size. Define a function that decides if a binary tree is complete.
-}

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

complete :: Tree a -> Bool
complete (Leaf v) = True
complete (Node x _ y) = sizet x == sizet y && complete x && complete y

foldt :: (a -> b) -> (b -> a -> b -> b) -> Tree a -> b
foldt f g (Leaf v)     = f v
foldt f g (Node x v y) = g (foldt f g x) v (foldt f g y)

sizet :: Tree a -> Int
sizet = foldt (const 1) (\ x a y -> x + y)
