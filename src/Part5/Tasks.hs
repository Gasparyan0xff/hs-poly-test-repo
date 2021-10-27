module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (l:lst) = myFoldl f (f acc l) lst

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (l:lst) = f l (myFoldr f acc lst)

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\l lst -> f l : lst) [] 

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = myFoldr ((++) . f) [] 

myConcat :: [[a]] -> [a]
myConcat = myFoldr (++) []

myReverse :: [a] -> [a]
myReverse lst = myFoldr (\l lst -> lst ++ [l]) [] lst

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\l lst -> if (p l) then l:lst else lst) []

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\l (s,t) -> if (p l) then (l:s, t) else (s, l:t)) ([], [])

