-- A type synonym:
type Pos = (Int, Int)
-- A type:
data Direction = North | South | East | West

-- 1
move :: Direction -> Pos -> Pos
move North (x, y) = (x, y+1)
move West  (x, y) = (x-1, y)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)

-- 2
moves :: [Direction] -> Pos -> Pos
moves []     pos = pos
moves (x:xs) pos = moves xs (move x pos)

-- 3
data Nat = Zero | Succ Nat deriving (Eq, Show, Read, Ord)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ $ int2nat $ x-1

-- 4
data Tree' = Leaf' | Node' Int Tree' Tree' deriving (Eq, Show, Read, Ord) 

insert' :: Int -> Tree' -> Tree'
insert' n Leaf' = Node' n Leaf' Leaf'
insert' n (Node' x left right)
  | n > x     = Node' x left (insert' n right)
  | n < x     = Node' x (insert' n left) right
  | otherwise = Node' x left right

-- 5
data Tree y = Leaf | Node y (Tree y) (Tree y) deriving (Eq, Show, Read, Ord) 

insert :: Ord y => y -> Tree y -> Tree y
insert n Leaf = Node n Leaf Leaf
insert n (Node x left right)
  | n > x     = Node x left (insert n right)
  | n < x     = Node x (insert n left) right
  | otherwise = Node x left right

-- 6
data Expr = Con Int
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Sub Expr Expr
     deriving (Eq, Show, Read, Ord)

value :: Expr -> Int
value (Con n)   = n
value (Add x y) = value x + value y
value (Mul x y) = value x * value y
value (Sub x y) = value x - value y
value (Div x y) = value x `div` value y
-- I use integer division
-- There is no min nor max

-- 7
print' :: Expr -> String
print' (Con n)   = show n
print' (Add x y) = "(" ++ print' x ++ " + " ++ print' y ++ ")"
print' (Mul x y) = print' x ++ " * " ++ print' y
print' _ = "..."

-- Morse Code
encode :: String -> String
encode []     = ""
encode (x:xs)
  | x == 'A'  = ".-"   ++ encode xs
  | x == 'B'  = "-..." ++ encode xs
  | x == 'C'  = "-.-." ++ encode xs
  | x == 'D'  = "-.."  ++ encode xs
  | otherwise = ""

-- Missing: decode

-- Type classes
class Sizeable t where
  size :: t -> Int

instance Sizeable Int where
  size _ = 1

instance Sizeable Char where
  size _ = 1

instance Sizeable x => Sizeable (x, y) where
  size (x, y) = size x + size x

instance Sizeable y => Sizeable (Tree y) where
  size Leaf = 0
  size (Node x left right) = size x + size left + size right

-- instance Sizeable [a] where
--   size [] = 0
--   size (x:xs) = 1 + size xs

instance Sizeable a => Sizeable [a] where
  size [] = 0
  size (x:xs) = size x + size xs
