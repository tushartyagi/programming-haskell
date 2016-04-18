import Data.Char

find :: (Eq a) => a -> [(a, b)] -> [b]
find k d = [v | (k', v) <- d, k' == k] -- find key in dict

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x' == x]

lowers :: String -> Int
lowers xs = length [x | x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x' == x]

-- 1
squareSum n = sum [n'^2 | n' <- [1..n]]

-- 2 by recursion
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n element = element : (replicate' (n - 1) element)

-- 2 by list comprehension
replicate'' :: Int -> a -> [a]
replicate'' n element = [element | _ <- [1..n]]

-- 3
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | z <- [1..n], x <- [1..z], y <- [1..z], x^2 + y^2 == z^2]

-- 4
factors :: Int -> [Int]
factors n = [x | x <- [1..m], n `mod` x == 0]
    where m = n - 1  -- Do not include the number

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], x == sum (factors x)]

-- 5
-- After banging my head at this, I was unable to find a way to solve this 
-- exercise using *only* generators and comprehensions. After 50 mins or so 
-- of thinking, I went ahead by using zip and replicate:
matrix = concat [zip (replicate 3 y) a | y <- b]
 where a = [4..6]
       b = [1..3]

-- 6
positions' :: (Eq a) => a -> [a] -> [Int]
positions' e xs = find e (zip xs [0..])

-- 7
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum (map productPair (zip xs ys))
    where productPair (x, y) = x * y

