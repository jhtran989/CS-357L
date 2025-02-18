{-
CS 357
Homework 5

John Tran
-}

-- Problem 1 Expression Tree

type Identifier = String

data Expr = Num Integer
          | Var Identifier
          | Let { var :: Identifier, value :: Expr, body :: Expr }
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

type Env = Identifier -> Integer

emptyEnv :: Env
emptyEnv = \s -> error ("unbound " ++ s)

extendEnv :: Env -> Identifier -> Integer -> Env
extendEnv oldEnv s n = \s' -> if s == s' then n else oldEnv s'

myEnv :: Env
myEnv s = read s :: Integer

{-
Part 1: instance type Expr into Show
-}

{-
Sample test cases
>>> 1 + 2
>>> var a
>>> value a
>>> body a
>>> emptyEnv "x"
>>> extendEnv myEnv "x" 1 "123"
>>> show (Num 1)
>>> show (Var "x")
>>> show (Let "x" (Num 3) (Add (Var "x") (Num 5)))
>>> show (Add (Num 1) (Add (Num 2) (Var "x"))) 
>>> show (Sub (Num 1) (Sub (Num 2) (Var "x"))) 
>>> show (Mul (Num 1) (Mul (Num 2) (Var "x"))) 
>>> show (Div (Num 1) (Div (Num 2) (Var "x"))) 
>>> show (show 1)
3
"a"
1
x
unbound x
123
"1"
"x"
"let x = 3 in x + 5 end"
"1 + 2 + x"
"1 - 2 - x"
"1 * 2 * x"
"1 / 2 / x"
"\"1\""
-}

instance Show Expr where
    show (Num a) = show a
    show (Var a) = a
    show (Let var value body) = "let " ++ var ++ " = " ++ (show value) ++ " in " ++ (show body) ++ " end"
    show (Add a b) = (show a) ++ " + " ++ (show b)
    show (Sub a b) = (show a) ++ " - " ++ (show b)
    show (Mul a b) = (show a) ++ " * " ++ (show b)
    show (Div a b) = (show a) ++ " / " ++ (show b)
{-
Need to NOT show the type Identifier since it is already a String

Also, could not get the "show [some Expr]" to display without quotes...but everything else should match. This makes sense since evaluating just "[some Expr]" without show will ALSO implicitly call show...and there is no level under no quotes so that "show [some Expr]" will not wrap everything in quotes
-}

{-
Part 2: define "evalInEnv" below which computes the arithmetic value corresponding the to the given Expr in
the given Env.
-}

{-
Sample test cases
>>> eval (Let "x" (Num 3) (Add (Var "x") (Num 5)))
>>> eval (Let "x" (Num 1) (Let "x" (Var "x") (Add (Var "x") (Num 1))))
>>> eval (Let "x" (Var "x") (Var "x"))
8
2
unbound x
-}

a = Let "a" (Num 1) (Var "x")

evalInEnv :: Env -> Expr -> Integer
evalInEnv env expr = case expr of
    (Num a) -> a
    (Var a) -> env a
    (Let var value body) -> evalInEnv (extendEnv env var (evalInEnv env value)) body
    (Add a b) -> (evalInEnv env a) + (evalInEnv env b)
    (Sub a b) -> (evalInEnv env a) - (evalInEnv env b)
    (Mul a b) -> (evalInEnv env a) * (evalInEnv env b)
    (Div a b) -> (evalInEnv env a) `div` (evalInEnv env b)
{-
Problem was SUPER VAGUE 
type Env was a helper type, which could have been explained a little more

Assuming integer arithmetic, so integer division

Had a lot of trouble with extendEnv...which seemed like it was only used in Part 2 -- at first, though we had to instance show for the type Env, which didn't make sense

need to recurse on any parts that were expressions

extendEnv only for Let...to get the binding of the variable with the corresponding value (replace the existing environment) so the value could be substituted in the "body" portion of the Expr
-}

eval :: Expr -> Integer
eval e = evalInEnv emptyEnv e

-- Problem 2 Infinite Lists and Diagonalization

{-
Sample Cases
>>> a = [[1,2], [1], [1, 2, 3]]
>>> a
[[1,2],[1],[1,2,3]]
>>> take 20 (diag qlist2)
["1","2","1/2","3","1","1/3","4","3/2","2/3","1/4","5","2","1","1/2","1/5","6","5/2","4/3","3/4","2/5"]
-}

getRowCol :: [a] -> Int -> a
getRowCol xs index = head (drop index xs)
{-
Gets the row/col of a given list (basically, creating our own indexing of lists). Since we assume we are working with a DOUBLY nested list, the first application will get the row at the specified index, and the second application will get the element of a given row at the specified index.

[indexing starts from 0]
Example:
(First application)
getRowCol [[1, 2, 3], [2, 3, 4]] 0 ==> [1, 2, 3]

(Second application)
getRowCol [1, 2, 3] 0 ==> 1
-}

getElement :: [[a]] -> (Int, Int) -> a
getElement xs (x, y) = getRowCol (getRowCol xs x) y
{-
Gives a nicer form to access a specific element of a doubly nested list by applying "getRowCol" twice, first at the given row, then the given column.

[indexing starts from 0]
(x, y) 
x - row index
y - column index

Example:
getElement [[1, 2, 3], [4, 5, 6]] (1, 2) ==> 6
-}

diag :: [[a]] -> [a]
diag xs = [getElement xs (rowIndex, columnIndex) | let infiniteIndex = [0..], diagIndex <- infiniteIndex, rowIndex <- [0..diagIndex], let columnIndex = diagIndex - rowIndex]
{-
Two nested loops where the outer loop is assumed to be infinite (diagonal index, starting at 0) and the inner loop will add all diagonal elements to the list (from right to left, as described in the hw spec.). 
-}

rlist :: [[Double]]
rlist = [ [i / j | i <- [1..] ] | j <- [1..] ]

qlist1 :: [[String]]
qlist1 = [ [ show i ++ "/" ++ show j | i <- [1..] ] | j <- [1..] ]

qlist2 :: [[String]]
qlist2 = [ [ fracString i j | i <- [1..] ] | j <- [1..] ]

-- Pretty print a fraction
fracString :: (Show p, Integral p) => p -> p -> String
fracString num den
    | denominator == 1 = show numerator
    | otherwise        = show numerator ++ "/" ++ show denominator
    where
        c           = gcd num den
        numerator   = num `div` c
        denominator = den `div` c

-- Take an n by n block from the top of a list of lists
block :: Int -> [[a]] -> [[a]]
block n xss = map (take n) (take n xss)

blockExample :: Bool
blockExample = block 5 qlist2 ==
    [
        ["1","2","3","4","5"],
        ["1/2","1","3/2","2","5/2"],
        ["1/3","2/3","1","4/3","5/3"],
        ["1/4","1/2","3/4","1","5/4"],
        ["1/5","2/5","3/5","4/5","1"]
    ]

diagTest :: Bool
diagTest = take 20 (diag qlist2) ==
    [
        "1",
        "2","1/2",
        "3","1","1/3",
        "4","3/2","2/3","1/4",
        "5","2","1","1/2","1/5",
        "6","5/2","4/3","3/4","2/5"
    ]

-- Problem 3 argmin

{-
Sample cases:
>>> abcd length ["asdf", "asdfdsf", "a", "sd"]
>>> abcde length ["asdf", "asdfdsf", "a", "sd"]
>>> abcde length [ "ABC", "DEF" ]
1
"a"
"ABC"
-}

argmin :: Ord a => (t -> a) -> [t] -> t
argmin f xs = undefined
-- abc f xs = sort [(fx, x) | (fx, x) <- zip (map f xs) xs]
abcd f xs = minimum [fx | (fx, x) <- zip (map f xs) xs]
-- abcde f xs = case (abcd f xs) of
--     fx | (fx == zx) -> z
--     _ | otherwise -> 
--     where 
--         ((zx, z):zs) = [(fx, x) | (fx, x) <- zip (map f xs) xs]
        
abcde f xs = go f zss 
    where 
        go f [] = []
        go f ((fx, x):xs) 
            | (fx == argMinValue) = x
            | otherwise = go f xs
        zss@((zx, z):zs) = [(fx, x) | (fx, x) <- zip (map f xs) xs]
        argMinValue = abcd f xs

myZipSort (fx1, x1) (fx2, x2) = compare fx1 fx2

-- Problem 4 DNA Sequences

{-
"bases" and "insertBase" are personal helper functions
-}
bases :: [String]
bases = ["A", "G", "C", "T"]

insertBase :: [a] -> [a] -> Int -> [a]
insertBase seq base index = (take index seq) ++ base ++ (drop index seq)

deleteBase :: [a] -> Int -> [a]
deleteBase seq index = (take index seq) ++ (drop (index + 1) seq)

substituteBase :: [a] -> [a] -> Int -> [a]
substituteBase seq base index = (take index seq) ++ base ++ (drop (index + 1) seq)


transposeBase :: [a] -> Int -> [a]
transposeBase seq index = (take index seq) ++ nextBase ++ prevBase ++ (drop (index + 2) seq)
    where 
        prevBase = [head (drop index seq)]
        nextBase = [head (drop (index + 1) seq)]
{-
Need to make sure to enclose each base in brackets so we can append them...

"prevBase" and "nextBase" refer to ordering relative to the ORIGINAL DNA sequence
-}


{-
Sample Cases
>>> drop 0 [1, 2, 3]
>>> take 0 [1, 2, 3]
>>> drop 3 [1, 2, 3]
>>> [0..(-1)]
[1,2,3]
[]
[]
[]
>>> insertions "GC"
["AGC","GAC","GCA","GGC","GGC","GCG","CGC","GCC","GCC","TGC","GTC","GCT"]
>>> deletions "AGCT"
["GCT","ACT","AGT","AGC"]
>>> substitutions "ACT"
["ACT","AAT","ACA","GCT","AGT","ACG","CCT","ACT","ACC","TCT","ATT","ACT"]
>>> transpositions "GATC"
["AGTC","GTAC","GACT"]
-}

insertions :: String -> [String]
insertions dnaSeq = [insertBase dnaSeq base index | base <- bases, let seqLen = length dnaSeq, index <- [0..seqLen]]
{-
The index goes from 0 to seqLen (not seqLen - 1) because we need indexing seqLen + 1 times (e.g., for "GC", the base could be inserted in index 0 (right before the seq), 1 (in the middle), or 2 (at the very end)).
-}

deletions :: String -> [String]
deletions dnaSeq = [deleteBase dnaSeq index | let seqLen = length dnaSeq, index <- [0..(seqLen - 1)]]
{-
The index goes from 0 to seqLen - 1 this time because we are just iterating through each base in the given DNA sequence (and removing the corresponding base)
-}

substitutions :: String -> [String]
substitutions dnaSeq = [substituteBase dnaSeq base index | base <- bases, let seqLen = length dnaSeq, index <- [0..(seqLen - 1)]]
{-
The index goes from 0 to seqLen - 1 this time because we are just iterating through each base in the given DNA sequence (and substituting the corresponding base)

Mix of "insertions" and "deletions", but adding another base in some existing spot instead of increasing (with a new base) or decreasing (removing a base entirely) the length of the DNA sequence, respectively.
-}

transpositions :: String -> [String]
transpositions dnaSeq = [transposeBase dnaSeq index | let seqLen = length dnaSeq, index <- [0..(seqLen - 2)]]
