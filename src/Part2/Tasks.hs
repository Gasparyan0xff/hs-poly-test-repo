module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) term1 term2 = BinaryTerm Plus term1 term2
infixl 1 |+|
(|-|) :: Term -> Term -> Term
(|-|) term1 term2 = BinaryTerm Minus term1 term2
infixl 1 |-|
(|*|) :: Term -> Term -> Term
(|*|) term1 term2 = BinaryTerm Times term1 term2
infixl 2 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar var repvar expr = case expr of 
                             Variable exprVar -> if (var == exprVar) then repvar else expr
                             BinaryTerm op lhv rhv -> BinaryTerm op (replaceVar var repvar lhv) (replaceVar var repvar rhv)
                             _ -> expr

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate expr = case expr of 
                BinaryTerm op lhv rhv -> let eval_lhv = evaluate(lhv) 
                                             eval_rhv = evaluate(rhv) in
                                         case (op, eval_lhv, eval_rhv) of
                                         (Plus, IntConstant 0, IntConstant t2) -> IntConstant t2
                                         (Plus, IntConstant t1, IntConstant 0) -> IntConstant t1
                                         (Plus, IntConstant t1,  IntConstant t2) -> IntConstant (t1 + t2)
                                         (Minus, IntConstant 0, IntConstant t2) -> IntConstant (-t2)
                                         (Minus, IntConstant t1, IntConstant 0) -> IntConstant t1
                                         (Minus, IntConstant t1, IntConstant t2) -> IntConstant (t1 - t2)
                                         (Times, IntConstant 0, IntConstant t2) -> IntConstant 0
                                         (Times, IntConstant t1, IntConstant 0) -> IntConstant 0
                                         (Times, IntConstant t1, IntConstant t2) -> IntConstant (t1 * t2)
                                         _ -> expr
                _ -> expr
