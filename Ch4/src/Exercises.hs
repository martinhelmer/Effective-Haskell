module Exercises where 


data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)

-- Turn a binary tree of strings into a pretty-printed string
showStringTree :: BinaryTree String -> String
showStringTree Leaf = " Leaf "
showStringTree (Branch t1 a t2) = a <> ": ( " <> showStringTree t1 <> showStringTree t2 <> ")"

-- Add a new integer into a binary tree of integers
addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree Leaf i = Branch Leaf i Leaf
addElementToIntTree (Branch t1 a t2) i 
    | t2 == Leaf = Branch t1 a (addElementToIntTree Leaf i)
    | otherwise = Branch (addElementToIntTree t1 i) a t2

-- Check to see if an int value exists in a binary tree of ints
doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False 
doesIntExist (Branch t1 a t2) i = 
    a == i || doesIntExist t1 i || doesIntExist t2 i




