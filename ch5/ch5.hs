import Data.Char

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]

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