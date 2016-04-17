-- 1
halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
    where half = length xs `div` 2

-- 2
safetail :: (Eq a) => [a] -> [a]
safetail xs = if null xs  
    then []
    else tail xs

safetail' :: (Eq a) => [a] -> [a]
safetail' xs | null xs = []
            | otherwise = tail xs

safetail'' :: (Eq a) => [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

-- 3
-- Calling the /\ operator
-- Method 1
--(!!!) :: Bool -> Bool
--True !!! True = True
--True !!! False = True
--False !!! True = True
--False !!! False = False

---- Method 2
--(!!!\') :: Bool -> Bool
--False !!!' False = False
--_ !!!' _ = True

---- Method 3
--(!!!'') :: Bool -> Bool
--True !!!'' b = True
--False !!!'' b = b

---- Method 4
--(!!!''') :: Bool -> Bool
--b !!!''' c | b == c = False
--      | otherwise = True

-- 4
-- Let's call it f
f :: Bool -> Bool -> Bool
f l r = if l then r
    else l

-- 5
-- Using and instead of && saved me some typing
f' :: Bool -> Bool -> Bool
f' l r = if and [l,r] then True
    else if and [not l,r] then False
    else if and [not r,l] then False
    else False


-- 6
mult :: Num a => a -> a -> a -> a
mult = \x -> \y -> \z -> x * y * z 
-- z creates a closure for y which has created the closure for x