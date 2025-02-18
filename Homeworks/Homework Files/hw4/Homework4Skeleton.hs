-- Problem 1 

data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Show, Eq)

balance :: [a] -> Tree a
balance = undefined

-- Problem 2

data T = Leaf | Node T T deriving (Eq, Show)

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

allpaths :: T -> [P]
allpaths = undefined

example2 :: T
example2 = Node Leaf (Node Leaf Leaf)

example3 :: T
example3 = Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf))

-- Problem 3

data Expr = Val Int | Add Expr Expr
    deriving (Eq, Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde = undefined

eval :: Expr -> Int
eval = undefined

exprExample1 :: Expr
exprExample1 = Add (Val 1) (Val 2)

exprExample2 :: Expr
exprExample2 = Add (Add (Val 1) (Val 2)) (Val 3)

-- Problem 4

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile = undefined

-- Problem 5

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan = undefined

-- Problem 6

combinations3 :: Ord a => [a] -> [[a]]
combinations3 = undefined

-- Problem 7

increasing :: Ord a => [a] -> Bool
increasing = undefined

-- Problem 8

combinations :: (Ord a, Integral b) => b -> [a] -> [[a]]
combinations = undefined

-- Problem 9

data Complex = Complex { real :: Integer, imaginary :: Integer }