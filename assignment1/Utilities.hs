module Utilities where

-- Takes a tuple of functions and a tuple of parameters.
-- Returns the value of the functions aplied on the parameters
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)

-- Takes a function and a value.
-- Returns Nothing if the value is Nothing else it returns the function aplied
-- on the value.
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)

-- Takes two values
-- Returns the second value if the first value is Nothing
-- else it returns the first.
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a

-- Takes a function and a value.
-- Tries to aply the function on the value and return
-- If fails it returns the value.
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)

-- Aplies the function on the value until it gives the same result as
-- the function was given.
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)

-- Takes a value from 0.0 - 1.0 and a list then gives back the coresponding
-- item of the index.
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs
