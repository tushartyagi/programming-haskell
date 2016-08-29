import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

-- -- IO_ type means that the action returns the type
-- type IO_ a = World -> (a, World)

-- getChar_ :: IO_ Char -- World -> (Char, World)

-- putChar_ :: Char -> IO_ ()  -- Takes in a Char and returns nothing

-- return_ :: a -> IO_ a
-- return_ v = \world -> (v, world)
