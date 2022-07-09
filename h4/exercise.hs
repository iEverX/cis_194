-- exercise 1

import Data.List ((\\))

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)


fun2' = sum . filter even . takeWhile (/=1) . iterate f
f x | x == 1 = 0
    | even x = div x 2
    | otherwise = 3 * x + 1


-- exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
     deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert v Leaf = Node 0 Leaf v Leaf
insert v (Node h left root right)
    | hei left < hei right = Node h (insert v left) root right
    | hei left > hei right = Node h left root (insert v right)
    | otherwise = let t = insert v left in Node (1+hei t) (insert v left) root right
    where
        hei Leaf = -1
        hei (Node h _ _ _) = h


-- exercise 3
xor :: [Bool] -> Bool
-- xor = odd . foldr (\x y -> if x then y + 1 else y) 0
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

-- ref: https://www.zhihu.com/question/37817937
myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f base xs =
    foldr (\x g -> \z -> g (f z x)) id xs base


-- exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x->2*x+1) . ([1..n] \\) . filter (<n) $ [ i + j + 2 * i * j | j <- [1..n], i <- [1..j]]
