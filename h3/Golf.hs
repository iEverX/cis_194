module Golf where

import Data.List

skips :: [a] -> [[a]]
skips a = map (uncurry f) (zip [1..length a] (repeat a))
f x = map snd . filter ((==0).(`mod` x).fst) . zip [1..]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = concat . map localM . take (length xs - 2) . tails $ xs
localM (x:y:z:_)
    | x < y && y > z = [y]
    | otherwise = []

--histogram = reverse . sort . map (\xs@(x:_) -> (length xs ,x)) . group . sort 
oneHis a = tail . concat . map seg $ zip (-1:a) (a++[10])
seg :: (Integer, Integer) -> String
seg (x,y) = '*':replicate (fromIntegral (y-x-1)) ' '


cntToValue = sort . map (\xs@(x:_) -> (length xs ,x)) . group . sort . filter (\z->z >= 0 && z < 10)
mit (x,y) [] = [(x,[y])]
mit (x,y) ((u,v):ns)
    | x == u = (u,y:v):ns
    | otherwise = (x,y:v):(u,v):ns
times = foldr mit [] . cntToValue
toLines = reverse . map (fmap (oneHis. sort)) . times

padLines ret [] = ret
padLines ret ((c,v):[]) = replicate c v ++ ret
padLines ret ((c,v):ns@((nc,_):_)) = replicate (c - nc) v ++ padLines ret ns

histogram :: [Integer] -> String
histogram = (++ "\n==========\n0123456789\n") . intercalate "\n" . padLines [] . toLines

eg = [2,3,5,1,1,1,3,0,9,8,8,8,8,8,8,3,15,23]
