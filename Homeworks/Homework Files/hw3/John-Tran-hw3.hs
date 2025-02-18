-- CS 357
-- Homework 3
-- John Tran
{-
Note:

For if statements, the "guard" form was used as it felt sytacticaly easier and better to use 

Also, this homework was completed before list comprehensions were introduced to us
-}
{-
Submission rules:

    - You must submit a single .hs file with the
    following name: \<firstName\>-\<lastName\>-hw3.hs.
    Failure to do so will result in -10 points.

    - You will lose 10 points if you put a module statement
    at the top of the file.

    - You will lose 10 points for any import statements you have
    in your file and will automatically miss any problems you used
    an imported function on.

    - If your file doesn't compile you will lose 10 points and miss any
    problems that were causing the compilation errors.

    - You must use the skeleton file (this file) provided and must not alter any type
    signature. If you alter a type signature you will automatically miss
    that problem.
-}

{-
Problem 1:
    Write a function stutter which takes a list of elements
    and returns a list where every element within the list
    is duplicated. For example:
>>> stutter []
[]

>>> stutter [1,2,3]
[1,1,2,2,3,3]

>>> stutter "Hello World"
"HHeelllloo  WWoorrlldd"
-}
stutter :: Eq a => [a] -> [a]
stutter ls = appendMap (\x -> [x, x]) ls
{-
now we can define the function f to just duplicate each element and put them in a list (the function is simple enough, so a lambda was used)
-}

appendMap :: (a -> [b]) -> [a] -> [b]
appendMap f ls = foldr (++) [] (map f ls)
{-
to keep the types consistent, we need [b] from f, so we have to define it above 
furthermore, we need to append each element of the new list (after applying the map with f) so they share the same nested level -- easiest way was to fold and append each element onto the empty list
-}


{- 
Problem 2:
    Write a function compress which takes in a list and eliminates
    all consecutive duplicate elements. For example:

>>> compress "HHeelllloo  WWoorrlldd"
"Helo World"

>>> compress [1,2,2,3,3,3,4,4,4,4,1,1]
[1,2,3,4,1]
-}
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : (compressHelper x xs)
{-
Calls the helper function below, but need to cons the first element (if nonempty) since the cons of a distinct element only happens on the second element of the comparison, skipping the very first element of the list
-}

compressHelper :: Eq a => a -> [a] -> [a]
compressHelper currElem [] = []
compressHelper currElem (x:xs)
    | (currElem == x) = compressHelper currElem xs
    | otherwise = x : compressHelper x xs
{-
A helper function used to keep track of the current element being compared with as the list is traversed, building up distinct elements onto a list
-}

{-
Another solution where an accumulator was used -- need to reverse the original list first because we can only cons to the beginning of the list
-}
-- compress ls = compress' (reverse ls)
--     where
--         compress' (x:xs) = compressHelper x [x] xs

-- compressHelper currElem acc [] = acc
-- compressHelper currElem acc (x:xs)
--     | (currElem == x) = compressHelper currElem acc xs
--     | otherwise = compressHelper x (x:acc) xs

{-
Problem 3
    Define a function isSuffixOf which takes two lists as arguments
    and returns True iff the first list is a suffix of the second.
    For example:

>>> isSuffixOf "bar" ""
False

>>> isSuffixOf "" "foo"
True

>>> isSuffixOf "bar" "foobar" 
True

>>> isSuffixOf "bar" "foobarf" 
False
-}
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = isSuffixOfHelper (reverse xs) (reverse ys)
{-
Calls the helper function below but makes sure that both lists are reversed since we want to check starting from the END of the list for the suffix (could of used the "last" function from the Prelude...)
-}

isSuffixOfHelper :: Eq a => [a] -> [a] -> Bool
isSuffixOfHelper [] [] = True
isSuffixOfHelper [] ys = True
isSuffixOfHelper xs [] = False
isSuffixOfHelper (x:xs) (y:ys)
    | (x == y) = isSuffixOfHelper xs ys
    | otherwise = False
{-
Check the first three combinations where any one of the lists are empty (first arg is the suffix) and then we check each character from the BEGINNING to see it there's an exact match (see isSuffixOf above to modify lists to check for suffix...)
-}


{-
Problem 4
    Define a function isHarshad which returns True iff a given
    number is Harshad number. A Harshad number is a number that is
    divisible by the sum of its digits. For example:

>>> isHarshad 7
True

>>> isHarshad 18
True

>>> isHarshad 11
False
-}
isHarshad :: Int -> Bool
isHarshad n = (mod n (sum (numToList n))) == 0
{-
We can check divisibility by using the "mod" function and checking if the remainder is 0
-}

numToList :: Integral a => a -> [a]
numToList n = numToListHelper n []

numToListHelper :: Integral a => a -> [a] -> [a]
numToListHelper 0 acc = acc
numToListHelper n acc =
            numToListHelper (div n 10) ((mod n 10) : acc)
{-
Format is a little different because we need an intermediate function to get all the digits of a number (numToList) and then we can check if the number is divisible by the sum of its digits

The above helper function used a similar method to the one of the problems shown on the first (Scheme) exam 
-}

{-
Problem 5
    Define a function mc91 which calculates the McCarthy 91 function.
    The function is defined as follows:

    MC(n)=
        n - 10         & if n >  100
        MC(MC(n + 11)) & if n <= 100

>>> map mc91 [95..110]
[91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
-}
mc91 :: Int -> Int
mc91 n
    | (n > 100) = n - 10
    -- n <= 100
    | otherwise = mc91 (mc91 (n + 11))
{-
Quite straightforward as it was just a translation of of the McCarthy 91 function into code with conditionals
-}

{-
Problem 6
    Write a function goldbach which when given an even number
    n returns a list of all pairs of primes which sum to n.
    Each pair in the list should be unique and the first prime in
    the pair should be smaller than the second. The list should
    also be in lexicographically sorted order.
    To write this function you will also need to write a function
    which tests primality. This function should be defined on the
    following range [0..]. For example:

>>> goldbach 6
[(3,3)]

>>> goldbach 20
[(3,17),(7,13)]

>>> goldbach 40
[(3,37),(11,29),(17,23)]

>>> goldbach 102
[(5,97),(13,89),(19,83),(23,79),(29,73),(31,71),(41,61),(43,59)]
-}
goldbach :: Int -> [(Int, Int)]
goldbach n =
    goldbachHelper n countBound 2
    where countBound = div n 2
{-
1 is NOT considered prime, so the tuple (1,n) is not included...

Starts from 2 instead

Edit: As stated in Slack, we could have implemented with or without using 2 as a valid prime. For this implementation, 2 IS considered a valid prime (could change the last arg of goldbachHelper above to 3 instead so the sequence starts checking at 3 and skips 2...)
-}

goldbachHelper :: Integral b => b -> b -> b -> [(b, b)]
goldbachHelper n countBound count
    | (count > countBound) = []
    | ((isPrime count) && (isPrime countComplement)) =
        (count, countComplement) : goldbachHelper n countBound (count + 1)
    | otherwise = goldbachHelper n countBound (count + 1)
    where countComplement = n - count
{-
Main helper function that keeps track of the integer division of n with 2 so we don't overcount cases and goes through numbers up to that bound (n/2) to check if both the current number (count) and its complement (countComplement) are both prime -- adds the tuple of those two to a list if that's the case
-}

isPrime :: Integral t => t -> Bool
isPrime n =
    let nBound = intSqrt n
    in
        isPrimeHelper n nBound 2
{-
Cannot start at 1 since everything is divisible by 1...
-}

isPrimeHelper :: Integral t => t -> t -> t -> Bool
isPrimeHelper n nBound count
    | (count > nBound) = True
    | (isDivisible n count) = False
    | otherwise = isPrimeHelper n nBound (count + 1)
{-
Checks all numbers up to the (integer) square root of the number to see if there are any factors that divides the number
-}

isDivisible :: Integral a => a -> a -> Bool
isDivisible dividend divisor =
    (mod dividend divisor) == 0
{-
Extracted from Problem 4 into a function that just checks if a number (dividend) is divisible by another number (divisor) -- check Problem 4 for more details
-}

intSqrt :: (Num t, Ord t) => t -> t
intSqrt n = intSqrtHelper n 0
{-
Starts the search at 0 (could have also start at 1)
-}

intSqrtHelper :: (Num t, Ord t) => t -> t -> t
intSqrtHelper n root
    | (root^2 > n) = root - 1
    | (root^2 == n) = root
    | otherwise = intSqrtHelper n (root + 1)
{-
Suprised there is not a built-in function to find the integer square root of a number...
Recursively increments the root until the square is either equal (then we found the exact root) to or greater than (take 1 less than the current root since we know the current root, if it divides the number, should have corresponding a factor LESS than the current root) the number itself
-}

{-
Problem 7
    Define a function increasing which takes a list and returns
    True iff the list is in increasing sorted order and False otherwise.
    For example:

>>> increasing [1,2,3]
True

>>> increasing "ABCD"
True

>>> increasing [100,99..0]
False
-}
increasing :: Ord a => [a] -> Bool
increasing xs = increasingHelper xs
{-
Essentially calling the helper function with the same args, so the helper might not be needed...
-}

increasingHelper :: Ord a => [a] -> Bool
increasingHelper [] = True
increasingHelper (y:[]) = True
increasingHelper (x:y:zs)
    | (y >= x) = increasingHelper (y:zs)
    | otherwise = False
{-
Few assumptions:
    - If the list given is empty OR has only one element, then it returns True
    - Else, checks the list for an increasing list ACCOUNTING FOR DUPLICATES -- e.g., [1, 1, 1, 2] would be considered increasing order (no specifics given in the problem statement besides increasing sorted order...)
-}

{-
Problem 8
    Define a function select which takes a predicate function and
    two lists and returns a list which contains elements from the second
    list where the predicate returned True when applied to the corresponding
    index in the first list. If the lists are not the same length then
    only consider indexes up to the length of the first list argument. For example:

>>> select even [1..26] "abcdefghijklmnopqrstuvwxyz"
"bdfhjlnprtvxz"

>>> select (<= 'g') "abcdefghijklmnopqrstuvwxyz" [1..26]
[1,2,3,4,5,6,7]

>>> select odd [1..10] "hello world this is joe"
"hlowr"
-}
select :: (a -> Bool) -> [a] -> [b] -> [b]
select pred is xs = 
    selectHelper pred is xs
{-
After condensing the code, the helper function wasn't needed after all...
-}

selectHelper :: (t -> Bool) -> [t] -> [a] -> [a]
selectHelper _ [] [] = []
selectHelper _ [] _ = []
selectHelper _ _ [] = []
selectHelper pred (is:iss) (x:xs)
    | (pred is) =
        x : selectHelper pred iss xs
    | otherwise = 
        selectHelper pred iss xs
{-
Originally wanted to keep of the min length (along with some counter index, like a loop) between the two lists as a separate parameter but realized that we could just check which list terminates first with pattern matching

Then actual body is ALMOST the same as a "filter," but we take the results from the other list instead, which we couldn't really edit as we did with Scheme...so the whole thing was coded up without using filter
-}

{-
Problem 9
    Define a function prefixSum which takes a list of numbers as its 
    argument and returns a list of sums of all prefixes of the list. 
    For example:

>>> prefixSum [1..10]
[1,3,6,10,15,21,28,36,45,55]

>>> prefixSum [2, 5]
[2,7]
-}
prefixSum :: [Int] -> [Int]
prefixSum xs = prefixSumHelper xs []
{-
Simple call to the helper function below...though it might have been clearer to call "reverse" here since the acc builds the prefix sum in REVERSE order
-}

prefixSumHelper :: Num a => [a] -> [a] -> [a]
prefixSumHelper (x:xs) [] = prefixSumHelper xs [x]
prefixSumHelper [] acc = reverse acc
prefixSumHelper (x:xs) acc =
    prefixSumHelper xs ((x + xAcc) : acc)
    where xAcc = head acc
{-
Not as hard as originally thought, but we definitely needed an accumulator for this one (actually, just needed to keep track of the most recent element, so we could have just used a separate parameter and cons everything together without reversing at the end...which we need to since we cons new elements at the HEAD, so everything will be in reversed order)

For our method using the accumulator, the most recent element would just be at the head since we cons new elements at the HEAD of the accumulator

From there, we just have two general cases: adding the first element to the list without any arithmetic and adding the current element in the list to the most recent element in the accumulator and cons it at the head of the accumulator
-}
    
