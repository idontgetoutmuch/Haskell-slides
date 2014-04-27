{-# LANGUAGE KindSignatures #-}

-- import Prelude hiding ( map, foldr )
import Data.List

-- 1. Example of asking the compiler for the type of an expression.
f x = x + 1

fact n | n == 0    = 1
       | otherwise = n * fact (n - 1)

-- 2. HM will give most general type. We might want to be more restrictive.

fact' n | n == 0    = 1
        | otherwise = n * fact (n - 1)

-- ?. Higher order function.

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x) : (map f xs)

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = foldr (\v vs -> (f v):vs) [] xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b

myFoldr f b [] = b
myFoldr f b (x:xs) = myFoldr f (f x b) xs

ones = 1 : ones

powersOf2 = 1 : map (*2) powersOf2

toBinary = unfoldr g
  where
    g :: Integer -> Maybe (Integer, Integer)
    g 0 = Nothing
    g n = Just (n `mod` 2, n `div` 2)

chunk = unfoldr g
  where
    g [] = Nothing
    g xs = Just (take 4 xs, drop 4 xs)

merge = unfoldr g
  where
    g :: ([a], [a], [a]) -> Maybe (a, ([a], [a], [a]))
    g ([], _, _) = Nothing
    g (_, [], _) = Nothing
    g (_, _, []) = Nothing
    g ((x:xs), ys, zs) = Just (x, (ys, zs, xs))

class MyFunctor (f :: * -> *) where
  myFmap :: (a -> b) -> f a -> f b

instance MyFunctor [] where
  myFmap = map

instance MyFunctor Maybe where
  myFmap f Nothing  = Nothing
  myFmap f (Just x) = Just (f x)

class MyFunctor m => MyMonad (m :: * -> *) where
  unit :: a -> m a
  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

instance MyMonad Maybe where
  unit x = Just x
  (f >=> g) x =
    case f x of
      Nothing -> Nothing
      Just y  -> g y

instance MyMonad [] where
  unit x = [x]
  (f >=> g) x = concatMap g (f x)

(>==) :: MyMonad m => m a -> (a -> m b) -> m b
m >== f = (const m >=> f) undefined

