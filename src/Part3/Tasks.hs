module Part3.Tasks where

import Data.List
import Util (notImplementedYet)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = [(f n)] ++ (finc f (n+1))  

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = [x] ++ (ff f (f(x)))

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
numberDiv :: Int -> [Int]
numberDiv num = let n = div num 10
                    m = mod num 10 in
                 case n of
                      0 -> [num]
                      _ -> [m] ++ numberDiv n

prepareFreq :: [Int] -> [Int]
prepareFreq lst = concat (map (numberDiv) lst)
numCount n lst = (length . filter (== n)) lst
calcFreq :: Int -> [Int] -> [Int]
calcFreq n lst | n > 9 = [0]
calcFreq n lst = (numCount n lst) : (calcFreq (n + 1) lst)
maxIndex :: [Int] -> Int
maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]
mostFreq :: [Int] -> Int
mostFreq lst =  maxIndex (calcFreq 0 (prepareFreq lst))

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq lst = let elem = head lst in
           elem : uniq (filter (/= elem) (tail lst))

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.
grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f l = let k = map (f) l
                 unique_list = nub k in
             map (\k -> (k, filter (\v -> f(v) == k) l)) unique_list