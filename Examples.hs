import Prelude hiding ( map )

-- 1. Example of asking the compiler for the type of an expression.
f x = x + 1

fact n | n == 0    = 1
       | otherwise = n * fact (n - 1)

-- 2. HM will give most general type. We might want to be more restrictive.

fact' n | n == 0    = 1
        | otherwise = n * fact (n - 1)

-- ?. Higher order function.

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x) : (map f xs)
