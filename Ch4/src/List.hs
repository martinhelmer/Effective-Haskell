module List where 
import Control.Applicative (Alternative(empty))

data List a = Empty | Cons a (List a) deriving (Show)

toList :: [a] -> List a
toList = foldr Cons Empty

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x l) = x:fromList l

fromList' :: List a -> [a]
fromList' = listFoldr (:) []

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ bb Empty = bb 
listFoldr f bb (Cons x xs) = f x $ listFoldr f bb xs

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ bb Empty = bb 
listFoldl f bb (Cons x xs) = listFoldl f (f bb x) xs

listHead :: List a -> Maybe a 
listHead Empty = Nothing
listHead (Cons x _) = Just x 

listTail :: List a -> List a
listTail Empty = Empty
listTail (Cons _ xs) = xs 

listReverse :: List a -> List a
listReverse Empty = Empty 
listReverse l = listFoldl (flip Cons) Empty l

listMap :: (a -> b) -> List a -> List b
listMap f = listFoldr (Cons . f) Empty