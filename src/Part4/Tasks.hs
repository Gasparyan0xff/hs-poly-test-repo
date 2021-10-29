module Part4.Tasks where

import Data.List
import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist = foldl (:<) REmpty


unionRlist :: ReverseList a -> ReverseList a -> ReverseList a
unionRlist REmpty REmpty = REmpty
unionRlist lst REmpty = lst
unionRlist REmpty lst = lst
unionRlist (lst1 :< last1) (lst2 :< last2) = let tmp = unionRlist (lst1 :< last1) lst2 in
                                             addRlist tmp last2
 
addRlist :: ReverseList a -> a -> ReverseList a
addRlist REmpty elem = (REmpty :< elem)
addRlist (lst :< last) elem = lst :< last :< elem

useComma :: ReverseList a -> String
useComma REmpty = ""
useComma (lst :< elem) = ","
showRlist :: (Show a) => (ReverseList a) -> String
showRlist REmpty = ""
showRlist (lst :< last) = showRlist(lst) ++ useComma(lst) ++ (show last)

showsRlist :: (Show a) => (ReverseList a) -> ShowS
showsRlist (l :< last) = (shows last) . showsRlist(l)

lengthRlist :: ReverseList a -> Int
lengthAccRlist acc (REmpty) = 0
lengthAccRlist acc (lst :< last) = acc + (lengthAccRlist acc lst)
lengthRlist REmpty = 0
lengthRlist (lst :< last) = 1 + lengthAccRlist 1 lst

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ lst = showsRlist lst
    show REmpty = "[]"
    show lst = "[" ++ showRlist lst ++ "]"
instance Eq a => Eq (ReverseList a) where
    (==) REmpty REmpty = True
    (==) REmpty (lst :< last) = False
    (==) (lst :< last) REmpty = False
    (==) (lst1 :< last1) (lst2 :< last2) =  if (lengthRlist lst1 /= lengthRlist lst2) then False
                                            else (last1 == last2) && ((==) lst1 lst2) 
    (/=) REmpty (lst :< last) = True
    (/=) (lst :< last) REmpty = True
    (/=) (lst1 :< last1) (lst2 :< last2) = ((/=) last1 last2) && not ((==) lst1 lst2)
instance Semigroup (ReverseList a) where
    (<>) REmpty REmpty = REmpty
    (<>) elem REmpty = elem
    (<>) REmpty elem = elem
    (<>) lst1 lst2 = unionRlist lst1 lst2
instance Monoid (ReverseList a) where
    mempty = REmpty
    mappend = (<>)
    mconcat = foldr mappend mempty
instance Functor ReverseList where
    fmap _ REmpty = REmpty
    fmap f (lst :< last) = fmap f lst :< (f last) 
instance Applicative ReverseList where
    pure e = REmpty :< e
    _ <*> REmpty = REmpty 
    REmpty <*> lst = REmpty
    (fs :< f) <*> (lst :< last) = unionRlist (fs <*> (lst :< last)) (fmap f lst :< (f last))
instance Monad ReverseList where
    return val = (REmpty :< val)
    REmpty >>= _ = REmpty
    (lst :< last) >>= f = unionRlist (lst >>= f) (f last)
    
    