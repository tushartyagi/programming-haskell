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

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f (x:xs) | f x == True = x : (takeWhile' f xs)
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f xs = takeWhile' (not . f) xs

-- 3
-- The parameters in lambda are the current value from the list and
-- the accumulated value.
-- Lambda has to return the new value of accumulator based on the
-- current input.
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x y -> f x : y) [] xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = foldr f [] xs
  where f v acc = if p v then v : acc else acc

-- 4
dec2int xs = foldl f 0 xs
  where f acc v = acc * 10 + v

-- 5
{-
I am not sure if compose [a] is really a function. No such thing on
hoogle. In any case, the following thing evaluates to 220:
sum . map (^2) . filter even $ [1..10] so not really sure what might be
the problem.
-}


