-- 1
-- This comprehension applies the function f on the elements 
-- which pass the predicate function p.
-- [f x | x <- xs, p x] == map f (filter p xs)

-- 2
all' :: (a -> Bool) -> [a] -> Bool
all' f xs = and (map f xs)

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f xs | length elements /= 0 = True
          | otherwise = False
          where elements = filter f xs

--takeWhile' :: (a -> Bool) -> [a] -> [a]
--takeWhile' f (x:xs) | f x == True 