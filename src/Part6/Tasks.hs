{-# LANGUAGE FlexibleInstances #-}
module Part6.Tasks where

import Util (notImplementedYet)
import Data.Map as M
import Data.Maybe (fromJust)

-- Разреженное представление матрицы. Все элементы, которых нет в sparseMatrixElements, считаются нулями
data SparseMatrix a = SparseMatrix {
                                sparseMatrixWidth :: Int,
                                sparseMatrixHeight :: Int,
                                sparseMatrixElements :: Map (Int, Int) a
                         } deriving (Show, Eq)

-- Определите класс типов "Матрица" с необходимыми (как вам кажется) операциями,
-- которые нужны, чтобы реализовать функции, представленные ниже
class Matrix mx where
      build :: Int -> Int -> mx
      get :: Int -> Int -> mx -> Int
      set :: Int -> Int -> Int -> mx -> mx
      insertval :: Int -> Int -> Int -> mx -> mx
      mult :: mx -> mx -> mx
      eyemat :: Int -> mx
      zeromat :: Int -> Int -> mx
      -- transpose
      
replaceList p f xs = [ if i == p then f x else x | (x, i) <- zip xs [0..] ]
replace2D v (x,y) ll = replaceList y (replaceList x (const v)) ll
replace2DList l x ll = replaceList x (const l) ll

dotVec v1 v2 = Prelude.foldl (+) 0 (zipWith (*) v1 v2)
multVecMat v m = Prelude.map (\l -> dotVec v l) m
multMat m1 m2 = Prelude.map (\l -> multVecMat l m1) m2


insertValList i val ll = let lst = ll !! i 
                             newlst = lst ++ [val] in
                         replace2DList newlst i ll
 
insertEye maxidx idx idxval afterWrite m = if (idx == maxidx) then m
                                           else 
                                                 if (idx == idxval && (not afterWrite)) then
                                                    let newmat = insertValList idx 1 m in
                                                    insertEye maxidx (idx+1) idxval True newmat
                                                 else 
                                                     let newmat = insertValList idx 0 m in
                                                     insertEye maxidx (idx+1) idxval afterWrite newmat
insertMatEye :: Int -> Int -> [[Int]] -> [[Int]]                                              
insertMatEye size idx mat | size == idx = mat
                          | otherwise = let newmat = insertEye size 0 idx False mat in
                                         insertMatEye size (idx+1) newmat

getZeros :: Int -> [[Int]]
getZeros n = [replicate n 0]

insertMatZero :: Int -> Int -> Int -> [[Int]] -> [[Int]]
insertMatZero w h i m | i == h = m
                      | otherwise = let lzero = replicate w 0
                                        newmat = replace2DList lzero i m in
                                        insertMatZero w h (i+1) newmat

-- Определите экземпляры данного класса для:
--  * числа (считается матрицей 1x1)
--  * списка списков чисел
--  * типа SparseMatrix, представленного выше
instance Matrix Int where
       build _ _ = 0
       get _ _ m = m
       set _ _ val _ = val
       insertval _  _ val _ = val
       mult l r = l * r
       eyemat w = 1
       zeromat _ _ = 0
instance Matrix [[Int]] where
       build _ 1 = [[]]
       build w h = (build w (h - 1)) ++ [[]]
       get i j m = (m !! i) !! j
       set i j val m = replace2D val (i, j) m
       insertval i _ val m = insertValList i val m
       mult l r = multMat l r
       eyemat w = let mat = build w w in
                      insertMatEye w 0 mat
       zeromat w h = let mat = build w h in
                         insertMatZero w h 0 mat
instance Matrix (SparseMatrix Int) where
       build w h = SparseMatrix w h M.empty
       get i j m = fromJust (M.lookup (i,j) $ (sparseMatrixElements m))
       set i j val m = let newmap = M.adjust (const val) (i,j) $ (sparseMatrixElements m)
                           w = sparseMatrixWidth m
                           h = sparseMatrixHeight m in
                       SparseMatrix w h newmap
       insertval i j val m = let newmap = M.insert (i,j) val (sparseMatrixElements m)
                                 w = sparseMatrixWidth m
                                 h = sparseMatrixHeight m in
                        SparseMatrix w h newmap
       mult l r = notImplementedYet
       eyemat w = let m = build w w in
                      insertSMatEye w 0 0 m
       zeromat w h = build w h

insertSMatEye :: Int -> Int -> Int -> SparseMatrix Int -> SparseMatrix Int
insertSMatEye size i j mat | size == i && size == j = mat
                           | otherwise = let newmat = insertval i j 1 mat in
                                         insertSMatEye size (i+1) (j+1) newmat

-- Реализуйте следующие функции
-- Единичная матрица
eye :: Matrix m => Int -> m
eye w = eyemat w

-- Матрица, заполненная нулями
zero :: Matrix m => Int -> Int -> m
zero w h = zeromat w h

-- Перемножение матриц
multiplyMatrix :: Matrix m => m -> m -> m
multiplyMatrix = mult

-- Определитель матрицы
determinant :: Matrix m => m -> Int
determinant = notImplementedYet
