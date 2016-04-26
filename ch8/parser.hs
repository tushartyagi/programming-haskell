-- the type which parses a string and returns a list.
-- [] denotes failure to parse the string
type Parser a = String -> [(a, String)]

-- Three types of basic parsers
-- returns the value v without consuming any input string
retrn :: a -> Parser a
retrn v = \input -> [(v, input)]

-- fails, no matter what the input
failure :: Parser a
failure = \input -> []

-- Returns the first character of the string; fails in
-- case the string is empty.
item :: Parser Char
item = \input -> case input of
  [] -> []
  (x:xs) -> [(x,xs)]

parse :: Parser a -> String -> [(a, String)]
parse p input = p input

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \input -> case p input of
  [] -> parse q input
  [(v, out)] -> [(v, out)]

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \input -> case parse p input of
  [] -> []
  [(v, out)] -> parse (f v) out

-- Bah! This won't compile. :| 
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then retrn x else failure
