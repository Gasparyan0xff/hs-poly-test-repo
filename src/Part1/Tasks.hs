module Part1.Tasks where

import Data.Fixed
import Util(notImplementedYet)
 

fact :: Double -> Double
fact 0 = 1
fact 1 = 1
fact n = n * fact(n - 1)

wrapMax :: (Double, Double) -> Double
wrapMax (x, max) = let tmp = max + (mod' x max) in
                (mod' tmp max)

wrapMinMaxPi :: (Double, Double, Double) -> Double
wrapMinMaxPi (a, min, max) = min + wrapMax(a - min, max - min)

-- синус числа (формула Тейлора)
mySin :: Double -> Double
_sin a = a - ((a**3)/fact(3)) + ((a**5)/fact(5)) - ((a**7)/fact(7)) + ((a**9)/fact(9)) - ((a**11)/fact(11)) + ((a**13)/fact(13))
mySin a = let x = wrapMinMaxPi(a, -pi, pi) in
          _sin(x)

-- косинус числа (формула Тейлора)
myCos :: Double -> Double
_cos a = 1 - ((a**2)/fact(2)) + ((a**4)/fact(4)) - ((a**6)/fact(6)) + ((a**8)/fact(8)) - ((a**10)/fact(10)) + ((a**12)/fact(12)) - ((a**14)/fact(14))
myCos a = let x = wrapMinMaxPi(a, -pi, pi) in
          _cos(x)
          

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = let l = abs a in
            let r = abs b in
            if (l == r) then l
            else if (l == 0) then r
            else if (r == 0) then l
            else myGCD r (mod l r)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
_isLeap :: Integer -> Bool
_isLeap y = isDivide 400 || (isDivide 4 && not (isDivide 100))
           where isDivide x = mod y x == 0
days = [31,28,31,30,31,30,31,31,30,31,30,31]
days_size = length days
getDay :: Int -> Integer -> Integer
getDay m y = if (_isLeap y && m == 2) then 29
             else days !! (m - 1)

isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect d m y = if (y < 1970) then False
                      else let _m = fromIntegral m in
                      if (_m > days_size) || (_m < 0) then False
                      else let _d = getDay _m y in
                           if (d > _d) || (d < 0) then False
                           else True 

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow x y = if (y == 0) then 1
            else if (y == 1) then x
            else x * myPow x (y - 1)

-- является ли данное число простым?
_isPrime :: Integer -> Integer -> Bool
_isPrime i n = if (n <= 2) 
                  then if (n == 2) then True
                       else False
               else if (mod n i == 0) then False
               else if (i * i > n) then True
               else _isPrime (i + 1) n 

isPrime :: Integer -> Bool                
isPrime n = (_isPrime 2 n)

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
--_mult_left :: Int -> Int -> [Point2D] -> Double 
_mult_left n i lst = if (i == (n - 1)) then fst(lst !! (n - 1)) * snd(lst !! 0)
                     else fst(lst !! i) * snd(lst !! (i + 1)) + (_mult_left n (i + 1) lst)
--_mult_right :: Int -> Int -> [Point2D] -> Double
_mult_right n i lst = if (i == (n - 1)) then fst(lst !! 0) * snd(lst !! (n - 1))
                      else fst(lst !! (i + 1)) * snd(lst !! i) + (_mult_right n (i + 1) lst)

shapeArea points = (0.5 * abs((_mult_left (length points) 0 points) - (_mult_right (length points) 0 points))) 

-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
{-
_greaterBound a b c = if (a > b && a > c) then 0
                      else if (b > a && b > c) then 1
                      else 2
__orderBound i a b c = if (i == 0) then [a, b, c]
                       else if (i == 1) then [b, a, c]
                       else [c, a, b]
-}
triangleKind a b c = if (a < b+c && b < a+c && c < b+c)
                     then let c_2 = c**2 in
                          let a_2 = a**2 in
                          let b_2 = b**2 in
                          if (c_2 == b_2+a_2) then 2
                          else if (c_2 < b_2+a_2) then 1
                          else 0
                     else -1