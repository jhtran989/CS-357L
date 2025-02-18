{-
CS 357
Homework 4

John Tran
-}

-- Problem 1 

{-
Sample test cases
>>> balance [1,2]
>>> balance [1,2,3]
>>> balance [1,2,3,4]
>>> balance [1,2,3,4,5,6]
NodeT (LeafT 1) (LeafT 2)
NodeT (LeafT 1) (NodeT (LeafT 2) (LeafT 3))
NodeT (NodeT (LeafT 1) (LeafT 2)) (NodeT (LeafT 3) (LeafT 4))
NodeT (NodeT (LeafT 1) (NodeT (LeafT 2) (LeafT 3))) (NodeT (LeafT 4) (NodeT (LeafT 5) (LeafT 6)))
-}

data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Show, Eq)

balance :: [a] -> Tree a
balance [x] = LeafT x
balance xs =
    NodeT (balance (take floorHalf xs)) (balance (drop floorHalf xs))
    where floorHalf = div (length xs) 2
{-
This problem was actually quite trivial once we exploited the binary property of the balanced tree (i.e., recurse on both halves of the list) and looking up how to take the complement of "take" (which we found was "drop") to determine the lengths of the two halves at each recursion step

An important thing to note is that if the length of the list at any recursion step is ODD, then the left half will take the floor (i.e., lesser of the halves) and the right half will take what is left over (i.e., greater of the halves)

DOES NOT WORK WITH A LIST WITH LESS THAN TWO ELEMENTS
-}

-- Problem 2

{-
Sample test cases
>>> allpaths example2
>>> allpaths example3
[This,GoLeft This,GoRight This,GoLeft (GoRight This),GoRight (GoRight This)]
[This,GoLeft This,GoLeft (GoLeft This),GoRight (GoLeft This),GoRight This,GoLeft (GoRight This),GoRight (GoRight This),GoLeft (GoRight (GoRight This)),GoRight (GoRight (GoRight This))]

>>> allpathsAlternate example2
>>> allpathsAlternate example3
[This,GoLeft This,GoRight This,GoRight (GoLeft This),GoRight (GoRight This)]
[This,GoLeft This,GoLeft (GoLeft This),GoLeft (GoRight This),GoRight This,GoRight (GoLeft This),GoRight (GoRight This),GoRight (GoRight (GoLeft This)),GoRight (GoRight (GoRight This))]
-}

{-
IMPORTANT: see the end for an alternate version that matches the given sample output
-}
data T = Leaf | Node T T deriving (Eq, Show)
getLeftTree :: T -> T
getLeftTree (Node t1 t2) = t1
getRightTree :: T -> T
getRightTree (Node t1 t2) = t2

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

allpaths :: T -> [P]
allpaths tree = allpathsHelper tree This

allpathsHelper :: T -> P -> [P]
allpathsHelper Leaf currPath = [currPath]
allpathsHelper currTree currPath = [currPath] ++ allpathsHelper (getLeftTree currTree) (GoLeft currPath) ++ allpathsHelper (getRightTree currTree) (GoRight currPath)
{-
We assume "This" is the whole tree that starts at the ROOT, so the base case should return a path and we can append all paths together (so "This" would need to be provided at the initial function call) -- need to keep track of which subtree we still have to traverse and the path we have built so far.

Using "map" was not my initial thought when approaching this problem (as hinted in Slack), but we could use a similar approach as above and recurse on both halves of the binary tree

In order to build a list as we recurse, the easiest way was to append the current path along with the recursive calls onto the current list, instead of cons-ing each individual element (hard to use an accumulator with two separate recursive calls and this method will inevitably lead to the cons of the empty list multiple times...)

A special note for the helper call on a "Node" is to make sure the previous path is appended since a "Leaf" case would append the current path -- this was a nice way to append "This" without having to explicitly append in in the main function "allpaths" (already handled in the helper function)
-}

allpathsAlternate :: T -> [P]
allpathsAlternate Leaf = [This]
allpathsAlternate (Node left right) = [This] ++ (map GoLeft (allpathsAlternate left)) ++ (map GoRight (allpathsAlternate right))
{-
Just realized we could just pattern match the "Node" instead of creating separate functions to get the left and right subtrees...

If we read from left to right (instead of using the parantheses), then we would want to construct the path also from left to right. In order to do so, we would need some way to pull the "GoLeft" and "GoRight" outside of the recursive call while still applying them to all subpaths. Now if we used the hint, then "map" would make a lot of sense here as we could just have the recursive call on the list arg of "map".
-}

example2 :: T
example2 = Node Leaf (Node Leaf Leaf)

example3 :: T
example3 = Node (Node Leaf Leaf) (Node Leaf (Node Leaf Leaf))

-- Problem 3

{-
Sample test cases
>>> eval exprExample1
>>> eval exprExample2
3
6
-}

data Expr = Val Int | Add Expr Expr
    deriving (Eq, Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Add expr1 expr2) = g (folde f g expr1) (folde f g expr2)
folde f g (Val value) = f value

eval :: Expr -> Int
eval expr = folde id (+) expr
{-
Reading the directions carefully, this one was also rather trivial. The important thing we have to realize here is that "folde" behaves a little differently when dealing with the "Add" constructor and with the "Val" constructor (i.e., we have to recurse on the two args of "Add" whereas we can just return the value for "Val").

Then we just need to find the corresponding functions for "f" and "g" to do what we want (i.e., returning the value itself for "f" and adding the two numbers for "g")
-}

exprExample1 :: Expr
exprExample1 = Add (Val 1) (Val 2)

exprExample2 :: Expr
exprExample2 = Add (Add (Val 1) (Val 2)) (Val 3)

-- Problem 4

{-
Sample test cases
(taken from http://zvon.org/other/haskell/Outputprelude/takeWhile_f.html for the built-in "takeWhile")
>>> myTakeWhile (<3) []
>>> myTakeWhile (<3) [1,2,3,4,5]
>>> myTakeWhile (>3) [1,2,3,4,5]
>>> myTakeWhile odd [1,3,5,7,9,10,11,13,15,17]
>>> myTakeWhile (\x -> 6*x < 100) [1..20]
>>> myTakeWhile ('w'>) "hello world"
[]
[1,2]
[]
[1,3,5,7,9]
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
"hello "
-}

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred [] = []
myTakeWhile pred (x:xs)
    | (pred x) = x : myTakeWhile pred xs
    | otherwise = []
{-
Again, rather trivial. We just keep cons-ing the element that satisfies the predicating and terminate the checking if the predicate is not met (even if there are other elements later on in the list that satisfies the predicate).
-}

-- Problem 5

{-
Sample test cases:
>>> span (< 3) []
([],[])
>>> drop 1 [1]
[]
>>> mySpan (< 3) [1,2,3,4,1,2,3,4]
>>> mySpan (< 9) [1,2,3]
>>> mySpan (< 0) [1,2,3]
([1,2],[3,4,1,2,3,4])
([1,2,3],[])
([],[1,2,3])
-}

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan pred [] = ([], [])
mySpan pred ls =
    (whilePart, otherPart)
    where
        whilePart = myTakeWhile pred ls
        whileLen = length whilePart
        otherPart = drop whileLen ls
{-
Again, rather trivial. The only new part we need to worry about is the second part (rest of the list) where we could use the "drop" function used earlier on to remove the first few elements covered in the first part for the second.
-}

-- Problem 6

{-
Sample test cases
>>> [(x, y) | x <- [1..3], y <- [1..3]]
>>> combinations3 []
>>> combinations3 "ABCDE"
>>> combinations3 "EDCBA"
>>> combinations3 "AABCD"
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
[]
["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]
["EDC","EDB","EDA","ECB","ECA","EBA","DCB","DCA","DBA","CBA"]
["AAB","AAC","AAD","ABC","ABD","ACD","ABC","ABD","ACD","BCD"]
-}

combinations3 :: Ord a => [a] -> [[a]]
combinations3 [] = []
combinations3 ls = [combo | let aLen = (length ls), (a, aIndex) <- (zip ls [1..aLen]), let bLs = (drop aIndex ls),let bLen = (length bLs), (b, bIndex) <- (zip bLs [1..bLen]), c <- (drop bIndex bLs), let combo = [a, b, c]]
{-
Assume elements of the list can not be used more than once (only way for duplicates to appear in the combos is if there were duplicates in the original list -- e.g., "AABCD")

Note for hint given in Slack:
The check for "increasing" should NOT be used (e.g., ls = "EDCBA", then there would be no combinations produced -- "increasing" only applicable when the input list is ALREADY SORTED). So, combinations should only retain the relative order in the original list, not necessarily the ordering of the elements themselves.
-}

------------------------------------------

-- combinations3 [] = []
-- combinations3 ls = [combo | a <- ls, let lsB = (removeElement a ls), b <- lsB, c <- (removeElement b lsB), let combo = [a, b, c], increasing combo]

-- removeElement :: Eq t => t -> [t] -> [t]
-- removeElement element [] = []
-- removeElement element (x:xs)
--     | (x == element) = xs
--     | otherwise = x : removeElement element xs

------------------------------------------

-- combinations3 ls = [combo | a <- ls, b <- ls, c <- ls, let combo = [a, b, c], strictIncreasing combo]

-- strictIncreasing :: Ord a => [a] -> Bool
-- strictIncreasing [] = True
-- strictIncreasing ls = and [xPrev < xNext | (xPrev, xNext) <- zip (init ls) (tail ls)]
{-
Hint used from Slack. However, had to edit increasing so only strictly 
-}

-- Problem 7

{-
Sample test cases
>>> and [True, True, True]
True
>>> increasing []
>>> increasing [1,2,3]
>>> increasing [3,2,1]
>>> increasing [1,2,2,3]
True
True
False
True
-}

increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing ls = and [xPrev <= xNext | (xPrev, xNext) <- zip (init ls) (tail ls)]

{-
Again, rather trivial. The only thing I needed a refresher on was getting a list WITHOUT the last element (achieved with the "init" function). 

The general approach was getting the list of elements without the LAST element and comparing that with the corresponding list of elements without the FIRST element and AND-ing all the results together.
-}

-- Problem 8

{-
Sample test cases
>>> combinations 3 "EDCBA"
>>> combinations 3 "ABCDE"
>>> combinations 2 "ABCDE"
>>> combinations 1 "ABCDE"
["EDC","EDB","EDA","ECB","ECA","EBA","DCB","DCA","DBA","CBA"]
["ABC","ABD","ABE","ACD","ACE","ADE","BCD","BCE","BDE","CDE"]
["AB","AC","AD","AE","BC","BD","BE","CD","CE","DE"]
["A","B","C","D","E"]
-}

combinations :: (Ord a, Integral b) => b -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) =
    (map (x:) (subset (n - 1))) ++ (subset n)
    where subset n' = combinations n' xs

-- Problem 9

{-
Sample test cases
>>> Complex 1 2
>>> (Complex 1 2) == (Complex 1 2)
>>> (Complex 1 2) == (Complex 3 4)
>>> (Complex 1 2) + (Complex 3 4)
>>> (Complex 1 2) * (Complex 3 4)
1+2i
True
False
4+6i
-5+10i
-}

data Complex = Complex { real :: Integer, imaginary :: Integer }

instance Eq Complex where
    (Complex real imaginary) == (Complex real' imaginary') =
        real == real' && imaginary == imaginary'

instance Show Complex where
    show (Complex real imaginary) = (show real) ++ "+" ++ (show imaginary) ++ "i"

instance Num Complex where
    (Complex real imaginary) + (Complex real' imaginary') =
        Complex (real + real') (imaginary + imaginary')
    (Complex real imaginary) * (Complex real' imaginary') =
        Complex ((real * real') - (imaginary * imaginary')) ((real * imaginary') + (imaginary * real'))
