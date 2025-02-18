-- Problem 1 

data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Show, Eq)

balance :: [a] -> Tree a
balance [x] = LeafT x
balance ls =
    let (left, right) = splitAt (length ls `div` 2) ls
    in NodeT (balance left) (balance right)

-- Problem 2

data T = Leaf | Node T T deriving (Eq, Show)

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

allpaths :: T -> [P]
allpaths Leaf = [This]
allpaths (Node left right) = ([This]) ++ (map GoLeft (allpaths left)) ++ (map GoRight (allpaths right))

example2 :: T
example2 = Node Leaf (Node Leaf Leaf)

example3 :: T
example3 = Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf))

-- Problem 3

data Expr = Val Int | Add Expr Expr
    deriving (Eq, Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add expr1 expr2) = g (folde f g expr1) (folde f g expr2)

eval :: Expr -> Int
eval (Val n) = n
eval expr = folde id (+) expr

exprExample1 :: Expr
exprExample1 = Add (Val 1) (Val 2)

exprExample2 :: Expr
exprExample2 = Add (Add (Val 1) (Val 2)) (Val 3)

-- Problem 4

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile cond [] = []
myTakeWhile cond (x:xs) =
    if cond x
        then x : myTakeWhile cond xs
        else []

-- Problem 5

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan cond ls = (myTakeWhile cond ls, dropWhile cond ls)

-- Problem 6

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = (map (x:) subset) ++ subset 
    where subset = powerset xs

myHelper :: [[a]] -> [[a]]
myHelper [] = []
myHelper (x:xs) =
    if length x == 3
        then x : myHelper xs
        else myHelper xs

combinations3 :: Ord a => [a] -> [[a]]
combinations3 [] = []
combinations3 ls =
    let result = powerset ls
    in myHelper result

-- Problem 7

increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing (x:xs)
    | null xs = True
    | x <= head xs = increasing xs
    | otherwise = False

-- Problem 8
combinationsHelper :: Int -> [[a]] -> [[a]]
combinationsHelper k [] = []
combinationsHelper k (x:xs) =
    if length x == k
        then x : combinationsHelper k xs
        else combinationsHelper k xs

combinations :: (Ord a, Integral b) => b -> [a] -> [[a]]
combinations 0 _ = []
combinations k [] = []
combinations k ls = 
    let result = powerset ls
    in combinationsHelper (fromIntegral k) result

-- Problem 9

data Complex = Complex { real :: Integer, imaginary :: Integer }

instance Eq Complex where (Complex real imaginary) == (Complex real' imaginary') = real == real' && imaginary == imaginary'

instance Show Complex where show (Complex real imaginary) = show real ++ "+" ++ show imaginary ++ "i"

instance Num Complex where 
    (Complex real imaginary) + (Complex real' imaginary') = Complex (real + real') (imaginary + imaginary')
    (Complex real imaginary) * (Complex real' imaginary') =
        Complex ((real * real') - (imaginary * imaginary')) ((real * imaginary') + (imaginary * real'))
