-- 1
-- [Char]
-- (Char, Char, Char)
-- [(Bool, Char)]
-- ([Bool], [Char])
-- [[a] -> [a]] -- List of functions which map list to list

-- 2
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x, y)

double :: (Num a) => a -> a
double x = x * 2

-- palindrome :: (Char a) => [a] -> Bool
-- Got the above signature incorrect, because Char is a type,
-- not a typeclass. 
-- Signature should be:
palindrome :: (Eq a) => [a] -> Bool
palindrome xs = reverse xs == xs

-- Using the above logic, it can also be:
palindrome' :: String -> Bool
palindrome' xs = reverse xs == xs
-- Although this one restricts the type to string, whereas the 
-- above could be used with any type implementing Eq. It was 
-- provided by ghci ;)


twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 4
-- odd and even have the same type signature:
-- odd/even :: Integral a => a -> Bool
-- Can these be equal? And since the domain is infinite, we cannot know 2 functions 
-- with same type signatures may diverge for some extra large input.