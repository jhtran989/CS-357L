-- Problem 1 

data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Show, Eq)

splitAtMid :: [a] -> ([a],[a])
splitAtMid xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree a
balance [x] = LeafT x
balance xs = NodeT (balance l) (balance r)
  where (l, r) = splitAtMid xs

-- Problem 2

data T = Leaf | Node T T deriving (Eq, Show)

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

allpaths :: T -> [P]
allpaths Leaf = [This]
allpaths (Node l r) = chooseThis ++ chooseLeft ++ chooseRight
  where chooseLeft  = map GoLeft (allpaths l)
        chooseRight = map GoRight (allpaths r)
        chooseThis  = [This]

example2 :: T
example2 = Node Leaf (Node Leaf Leaf)

example3 :: T
example3 = Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf))

-- Problem 3

data Expr = Val Int | Add Expr Expr
    deriving (Eq, Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde = f g (Val n) = f n
folde = f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Expr -> Int
eval = undefined

exprExample1 :: Expr
exprExample1 = Add (Val 1) (Val 2)

exprExample2 :: Expr
exprExample2 = Add (Add (Val 1) (Val 2)) (Val 3)

-- Problem 4

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x:xs) | f x = x : myTakeWhile f xs
                   |otherwise = []

-- Problem 5

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan p xs = (myTakeWhile p xs, dropWhile p xs)

-- Problem 6

combinations3 :: Ord a => [a] -> [[a]]
combinations3 = undefined

-- Problem 7

increasing :: Ord a => [a] -> Bool
increasing []  = True
increasing [x] = True
increasing (x:y:xs) = x <= y && increasing (y:xs)

-- Problem 8

combinations :: (Ord a, Integral b) => b -> [a] -> [[a]]
combinations = undefined

-- Problem 9

data Complex = Complex { real :: Integer, imaginary :: Integer }