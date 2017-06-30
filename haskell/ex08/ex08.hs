
{-
Develop some functions to work with order lists. Make use of higher-order functions and/or recursion.
-}

type ArtName = String   -- name of article
type Number = Int       -- number of ordered articles
type Order = (ArtName, Number)

type Price = Int   -- price of an article in Rappen
type Pricing = (ArtName, Price)
type PricedOrder = (ArtName, Number, Price)

{-
Note: Order and Pricing are exactly the SAME type. However, we distinguish them on the software engineering level, but we must be careful.
-}

ol01 :: [Order]
ol01 =
  [("Schraube M4", 100),
   ("Mutter M4", 100),
   ("Unterlegscheibe M4", 200)]

pl01 :: [Pricing]
pl01 =
  [("Schraube M4", 5),
   ("Unterlegscheibe M4", 2),
   ("Mutter M4", 5),
   ("Zahnrad 36Z", 1300)]

{-
Given a name and a list of name item pairs, myLookup returns the first item in the list that matches the given name. If the list does not contain the given name, myLookup fails. Later we will write a better function that returns a value indicating whether an item has been found or not.
-}
myLookup :: Eq a => a -> [(a, b)] -> b
myLookup n = snd . head . take 1 . filter (\x -> fst x == n)

{-
Given an order list and a pricing list, addPrices adds the prices according to the pricing list to the order list.
Precondition: All article names in the order list occur in the pricing list.
-}
addPrices :: [Order] -> [Pricing] -> [PricedOrder]
addPrices ol pl = map (\(name, num) -> (name, num, myLookup name pl * num)) ol

addPrices' :: [Order] -> [Pricing] -> [PricedOrder]
addPrices' xs ys = zip3 [fst x | x <- xs] [snd x | x <- xs] [snd x * myLookup (fst x) ys | x <- xs]

addPrices'' :: [Order] -> [Pricing] -> [PricedOrder]
addPrices'' os ps = [(x, y, y * z) | (x, y, z) <- zipWith3 zipper os os os]
  where zipper xs ys zs = (fst xs, snd ys, myLookup (fst zs) ps)

{-
totalPrice determines the total price of an order list.
-}
totalPrice :: [PricedOrder] -> Price
totalPrice pol = sum (map (\(_, _, price) -> price) pol)

totalPrice' :: [PricedOrder] -> Price
totalPrice' = foldr ((+) . third) 0
  where third (_, _, x) = x

{-
totalNumPrice determines the total number of items and the total price of an order list.
-}
totalNumPrice :: [PricedOrder] -> (Number, Price)
totalNumPrice o = (totalNum o, totalPrice o)
  where totalNum = sum . map (\(_, x, _) -> x)

totalNumPrice' :: [PricedOrder] -> (Number, Price)
totalNumPrice' pol = (sum nums, sum prices)
  where (_, nums, prices) = unzip3 pol

{-
Returns items that (for the number ordered) cost more than a given maxPrice.
-}
tooExpensive :: Price -> [PricedOrder] -> [PricedOrder]
tooExpensive p = filter (\(_, _, x) -> x > p)

{-
Adds an order to an order list. If the article name added already occurs in the order list, the number is accordingly incremented.
-}
addOrder :: Order -> [Order] -> [Order]
addOrder (name, num) ((name', num') : ol)
  | name == name' = (name, num + num') : ol
  | otherwise     = (name', num') : addOrder (name, num) ol
addOrder nameNum [] = [nameNum]

{-
addOrderList adds all orders of a new order list to an old order list.
-}
addOrderList :: [Order] -> [Order] -> [Order]
addOrderList olNew olOld = foldr addOrder olOld olNew

addOrderList' :: [Order] -> [Order] -> [Order]
addOrderList' (x : xs) os = addOrderList xs $ addOrder x os
addOrderList' _ os        = os

{-
Removes an order with a given article name from a given order list.
-}
removeOrder' :: ArtName -> [Order] -> [Order]
removeOrder' n = filter (\(xn, _) -> n /= xn)
