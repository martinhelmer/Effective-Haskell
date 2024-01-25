module Peano where 


data Peano = Z | S Peano deriving (Show)


toPeano :: Int -> Peano
toPeano 0 = Z
toPeano n = S (toPeano $ n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano p)

eqPeano :: Peano -> Peano -> Bool 
eqPeano p p' =
    case (p,p') of
        (Z,Z) -> True
        (S n, S n') -> eqPeano n n' 
        _ -> False

addPeano :: Peano -> Peano -> Peano
addPeano s Z = s  
addPeano s (S t) = addPeano (S s) t 