double n = n + n
quadruple = double . double
factorial n = product [1..n]
average ns = sum ns `div` length ns

-- 1
-- (2 ^ 3) * 4
-- (2 * 3) + (4 * 5)
-- (2 + (3 * (4 ^ 5))) -- Lisp?

-- 3
-- function name cannot be capital letter
n = a `div` (length xs) -- without parens, the length is the second param to div
    where a = 10
          xs = [1..5]   -- indentation


-- 4
myLast xs = drop allButLast xs
    where allButLast = length xs - 1

-- Another approach could be pattern matching
myLast' (x:[]) = x
myLast' (x:xs) = myLast' xs

-- 5
myInit xs = take allButLast xs
    where allButLast = length xs - 1

myInit' (_:[]) = []
myInit' (x:xs) = x : myInit' xs
