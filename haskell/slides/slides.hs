
import System.IO
import Control.Applicative
import Data.Char

-- *******************
-- Slides: First Steps
-- *******************

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

-- *************************
-- Slides: Types And Classes
-- *************************

{-
Exercise 1:
What are the types of the following values?
-}

['a', 'b', 'c'] -- :: [Char]
('a', 'b', 'c') -- :: (Char,Char,Char)
[(False, '0'), (True, '1')] -- :: [(Bool,Char)]
([False, True], ['0', '1']) -- :: ([Bool],[Char])
[tail, init, reverse] -- :: [[a] -> [a]]

{-
Exercise 2:
What are the types of the following functions?
-}

second' :: [a] -> a
second' xs = head (tail xs)

swap' :: (t1,t) -> (t,t1)
swap' (x, y) = (y, x)

pair' :: t -> t1 -> (t,t1)
pair' x y = (x, y)

double' :: Num a => a -> a
double' x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

-- **************************
-- Slides: Defining Functions
-- **************************

{-
Exercise 1:
Consider a function safetail that behaves in the same way as tail, except that safetail maps the empty list to the empty list, whereas tail gives an error in this case. Define safetail using:
        1. a conditional expression.
        2. guarded equations.
        3. pattern matching.
Hint: the library function null :: [a] -> Bool can be used to test if a list is empty.
-}

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs
          | null xs   = []
          | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

{-
Exercise 2:
Give three possible definitions for the logical or operator (||) using pattern matching.
-}

(|||) :: Bool -> Bool -> Bool
True ||| _     = True
False ||| True = True
_ ||| _        = False

(||||) :: Bool -> Bool -> Bool
False |||| False = False
_ |||| _         = True

(|||||) :: Bool -> Bool -> Bool
True ||||| False  = True
True ||||| True   = True
False ||||| True  = True
False ||||| False = False

{-
Exercise 3:
Redefine the following version of (&&) using conditionals rather than patterns:
        True && True = True
        _ && _ = False
-}

(&&&) :: Bool -> Bool -> Bool
a &&& b = if a == True then
            if b == True then True else False
              else False

{-
Exercise 4:
Do the same for the following version:
        True && b = b
        False && _ = False
-}

(&&&&) :: Bool -> Bool -> Bool
a &&&& b = if a == True then b else
             if a == False then False else b

-- **************************
-- Slides: List Comprehension
-- **************************

{-
Exercise 1:
A triple (x, y, z) of positive integers is called pythagorean if x2 + y2 = z2. Using a list comprehension, define a function
        pyths :: Int -> [(Int, Int, Int)]
that maps an integer n to all such triples with components in [1 .. n]. For example:
        > pyths 5
        [(3,4,5),(4,3,5)]
-}

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]

{-
Exercise 2:
A positive integer is perfect if it equals the sum of all of its factors, excluding the number itself. Using a list comprehension, define a function
        perfects :: Int -> [Int]
that returns the list of all perfect numbers up to a given
limit. For example:
        > perfects 500
        [6,28,496]
-}

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], sum (factors x) - x == x]

factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

{-
Exercise 3:
The scalar product of two lists of integers xs and ys of length n is give by the sum of the products of the corresponding integers. Using a list comprehension, define a function that returns the scalar product of two lists.
-}

scalar :: Num a => [a] -> [a] -> a
scalar xs ys = sum [xs !! i * ys !! i | i <- [0 .. length xs - 1]]

-- ***************************
-- Slides: Recursive Functions
-- ***************************

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

-- ******************************
-- Slides: Higher-Order Functions
-- ******************************

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

-- ***********************************
-- Slides: Declaring Types And Classes
-- ***********************************

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

-- *******************************
-- Slides: Interactive Programming
-- *******************************

{-
Exercise:
Implement the game of nim in Haskell, where the rules of the game are as follows:

• The board comprises five rows of stars:

          1:* * * * *
           2:* * * *
            3:* * *
             4:* *
             5: *

• Two players take it turn about to remove one or more stars from the end of a single row.

• The winner is the player who removes the last star or stars from the board.

• Hint: Represent the board as a list of five integers that give the number of stars remaining on each row. For example, the initial board is [5, 4, 3, 2, 1].
-}

nim = play [1,2,3,4,5] ["Player 1","Player 2"]

play :: [Int] -> [String] -> IO ()
play board players = do
    (newBoard,winner) <- takeTurns board players
    if null winner
      then play newBoard players
      else do putStrLn (winner ++ " won!")
              return ()

takeTurns :: [Int] -> [String] -> IO ([Int],String)
takeTurns board [] = return (board,[])
takeTurns board (player:players) = do
    newBoard <- takeTurn board player
    if sum newBoard == 0
      then return (newBoard,player)
      else takeTurns newBoard players

takeTurn :: [Int] -> String -> IO [Int]
takeTurn board player = do
    putStrLn ("\n" ++ player ++ "'s Turn")
    displayBoard board
    row   <- getRow board
    count <- getInt "How many stars? " 1 (board!!row)
    return $ take row board ++ [board!!row - count] ++ drop (row+1) board

displayBoard :: [Int] -> IO ()
displayBoard [] = return ()
displayBoard board = do
    putStrLn $ show (length board) ++ " : " ++ replicate (last board) '*'
    displayBoard (init board)

getRow :: [Int] -> IO Int
getRow board = do
    row <- getInt "Which row? " 1 (length board)
    if board!!(row-1) == 0
      then do putStrLn "That row is empty!"
              getRow board
      else return (row-1)

getInt msg min max = do
    putStr msg
    input <- getLine
    let parsed = reads input :: [(Int,String)]
    if null parsed
      then badNumber "That's not a number!"
      else testNumber (fst (head parsed))
    where
        badNumber error = do putStrLn error
                             getInt msg min max
        testNumber number
            | number < min = badNumber "That number is too small."
            | number > max = badNumber "That number is too big."
            | otherwise    = do return number

-- **************************
-- Slides: Functional Parsers
-- **************************

{-
Exercise 1:
Why does factorising the expression grammar make the resulting parser more efficient?
-}

{-
Without left-factorising, the resulting parser would backtrack excessively and
take exponential time in the size of the expression. For example, a number would
be parsed four times before being recognised as an expression.
-}

{-
Exercise 2:
Extend the expression parser to allow the use of subtraction and division, based upon the following extensions to the grammar:

   expr → term ('+' expr | '-' expr | ε)
   term → factor ('*' term | '/' term | ε)

-}

newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
   fmap g p = P (\inp -> case parse p inp of
                            []        -> []
                            [(v,out)] -> [(g v, out)])

instance Applicative Parser where
   pure v = P (\inp -> [(v,inp)])
   pg <*> px = P (\inp -> case parse pg inp of
                             []        -> []
                             [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
   p >>= f = P (\inp -> case parse p inp of
                           []        -> []
                           [(v,out)] -> parse (f v) out)

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

failure :: Parser a
failure = P (\ inp -> [])

returnp :: a -> Parser a
returnp v = P (\ inp -> [(v,inp)])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\ inp -> case parse p inp of
               [] -> parse q inp
               [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then returnp x else failure

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char x = sat (x ==)

expr :: Parser Int
expr = do t <- term
          do char '+'
             e <- expr
             returnp (t + e)
             +++ do char '-'
                    e <- expr
                    returnp (t - e)
                    +++ returnp t

term :: Parser Int
term = do f <- factor
          do char '*'
             t <- term
             returnp (f * t)
             +++ do char '/'
                    t <- term
                    returnp (f `div` t)
                    +++ returnp f

factor :: Parser Int
factor = do d <- digit
            returnp (digitToInt d)
            +++ do char '('
                   e <- expr
                   char ')'
                   returnp e

eval :: String -> Int
eval xs = fst (head (parse expr xs))
