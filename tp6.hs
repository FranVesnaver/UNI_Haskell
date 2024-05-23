--1 
data Nat = Zero | Succ Nat 

--2 
natToInt :: Nat -> Int 
natToInt Zero = 0
natToInt (Succ x)  = 1+ natToInt x

--3
intToNat :: Int -> Nat
intToNat x | x == 0 = Zero
                   | x > 0 = Succ (intToNat (x-1))
                   
--4
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero Zero = Zero
sumaNat Zero (Succ x) = (Succ x)
sumaNat (Succ x) Zero = (Succ x)
sumaNat (Succ x) (Succ y) = sumaNat (Succ(Succ x)) y

multNat :: Nat -> Nat -> Nat
multNat Zero _ = Zero
multNat _ Zero = Zero
multNat m (Succ n) = sumaNat m (multNat m n)
--5 
instance Eq Nat where
  Zero == Zero = True
  (Succ x) == (Succ y) = x == y
  _ == _ = False
  
instance Ord Nat where
  Zero <= _ = True
  (Succ x) <= Zero = False
  (Succ x) <= (Succ y) = x <= y
  
instance Show Nat  where
 show n = show (natToInt n)
 
 --6
data Arbol a = Nil | Node (Arbol a) a (Arbol a)

--7
size :: Arbol a -> Int
size Nil = 0
size (Node hi r hd) = 1 + size hi + size hd

--8
height :: Arbol a -> Int 
height Nil = 0
height (Node hi r hd) = 1 + max (height hi) (height hd)

--9
instance Show a => Show (Arbol a) where
  show Nil = "<>"
  show (Node Nil r Nil) = "<" ++ show r ++ ">"
  show (Node hi r hd) = "<" ++ show hi ++ "," ++ show r ++ "," ++ show hd ++ ">"