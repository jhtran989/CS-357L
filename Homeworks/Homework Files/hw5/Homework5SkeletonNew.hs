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

evalInEnv :: Env -> Expr -> Integer
evalInEnv = undefined

eval :: Expr -> Integer
eval e = evalInEnv emptyEnv e

-- Problem 2 Infinite Lists and Diagonalization

diag :: [[a]] -> [a]
diag = undefined

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

argmin :: Ord a => (t -> a) -> [t] -> t
argmin = undefined

-- Problem 4 DNA Sequences

insertions :: String -> [String]
insertions = undefined

deletions :: String -> [String]
deletions = undefined

substitutions :: String -> [String]
substitutions = undefined

transpositions :: String -> [String]
transpositions = undefined