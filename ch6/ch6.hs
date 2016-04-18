insert :: (Ord a) => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                | otherwise = y : insert x ys

-- Insertion sort: Pick each element and insert it 
-- into its correct position.
isort :: (Ord a) => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

-- Quicksort: Pick each element and put it in a position
-- such that the elements to the left are smaller and 
-- elements to the right are larger.
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where smaller = [y | y <- xs, y <= x]
          larger  = [y | y <- xs, y >  x]

-- 1
pow :: Int -> Int -> Int
pow b 0 = 1
pow b n = b * (pow b (n - 1))

-- Leaving out 2

-- 3
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = [a] ++ (replicate' (n - 1) a)

nth :: Int -> [a] -> a
nth 0 (x:xs) = x
nth n (x:xs) = nth (n-1) xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = (a == x) || (elem' a xs)

-- 4 
merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge xl@(x:xs) yl@(y:ys) | x <= y = x : merge xs yl
                          | x > y  = y : merge xl ys

-- 5
mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort x | length x == 1 = x
            | otherwise = merge (mergesort firstHalf) (mergesort secondHalf)
            where (firstHalf, secondHalf) = halve x

mergesort' :: (Ord a) => [a] -> [a]
mergesort' [] = []
mergesort' [x] = [x]  -- Not sure if this is a singleton list or a full fledged list??
mergesort' x = merge (mergesort' firstHalf) (mergesort' secondHalf)
            where (firstHalf, secondHalf) = halve x

halve :: [a] -> ([a], [a])
halve xs = (take middle xs, drop middle xs)
    where len = length xs
          middle | even len = len `div` 2
                 | otherwise = (len + 1) `div` 2

