{-
  Exercise 2.1: Introduction to Haskell, recursively!
  Solve last week's exercises using recursion instead of built-in functions.
  Like last week, be careful to write the type signatures first.
  Watch out for hints!
  Hints may also apply to later constructions, keep them in mind.
  The stars indicate roughly the difficulty of the expected solution:
  * should be easy,
  ** is only slightly more complicated,
  *** means that you may have to pause and think for a bit,
  **** can be tricky; it's okay to get stuck on these, consider moving past
       them and returning to them at the end of the exercise set.
 -}

-- *
-- 'penultimateElement l' returns the second-to-last element of the list l
penultimateElement :: [a] -> a
penultimateElement [x,y] = x
penultimateElement (x:xs) = penultimateElement xs

-- **
-- 'append l1 l2' appends the list l2 at the end of l1, like the (++) operator

append :: [a] -> [a] -> [a]
append [] ys  =  ys
append (x:xs) ys = x: (append xs ys)


-- **
-- 'get k l' returns the k-th element in the list l
-- Example:
-- ghci> get 2 [0,0,1,0,0,0]
-- 1
get ::  Integer -> [a] -> a
get  1 (x:xs) = x  
get n (x:xs) = get (n-1) xs

-- **
-- 'double l' "doubles" the list l
-- Hint: You may use 'append'.
-- Example:
-- ghci> double [1,2,3,3]
-- [1,1,2,2,3,3,3,3]
double :: [a] -> [a]
double [] = []
double (x:xs) = append  [x,x] (double xs)

-- ***
-- 'divide k l' divides the list l into a pair of a list of the first k elements
-- of l and the rest
-- Example:
-- ghci> divide 2 [1,1,1,2,2,2]
-- ([1,1],[1,2,2,2])
divide :: Integer -> [a] -> ([a],[a])
divide 0 xs = ([], xs)
divide n (x:xs) = (x:begining, rest)
  where (begining, rest) = divide (n-1) xs

-- **
-- 'delete k l' returns the list l with the k-th element removed
-- Example:
-- ghci> delete 3 [0,0,0,1,0,0,0]
-- [0,0,0,0,0,0]

delete :: Integer -> [a] -> [a]
delete 1 (x:xs) = xs
delete n (x:xs) = x: delete (n-1) xs 

-- ***
-- 'slice i k l' returns the sub-list of l from the i-th up to (excluding) the k-th element
-- Hint: This is a recursion followed by another recursion. Define a local
--       auxiliary function.
-- Example:
-- ghci> slice 3 6 [0,0,0,1,2,3,0,0,0]
-- [1,2,3]
slice :: Integer -> Integer -> [a]-> [a]
slice 0 1 (x:xs) = [x]
slice i k (x:xs)
  |i>0 = slice (i-1) (k-1) xs
  |otherwise = x: slice i (k-1) xs

-- **
-- 'insert x k l' inserts x at index k into l
-- Example:
-- ghci> insert 2 5 [0,0,0,0,0,0]
-- [0,0,0,0,0,2,0]
insert :: a -> Integer -> [a] -> [a]
insert x 0 l = x : l
insert x k (l:ls) = l: insert x (k-1) ls

-- **
-- 'rotate n l' rotates l to the left by n places
-- Example:
-- ghci> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
rotate :: Integer -> [a]-> [a]
rotate 0 xs = xs
rotate n (x:xs) = rotate (n-1) (append xs [x])

-- **
-- 'remove x l' returns a l with *all* occurances of x removed
-- Example:
-- ghci> remove 'a' "abrakadabra"
-- "brkdbr"
remove :: (Eq a) => a -> [a] -> [a]
remove y [] = []
remove y (x:xs)
  |y /= x = x: remove y xs
  | otherwise = remove y xs


-- **
-- 'reverseLength lst' computes a couple of the reversed list of 'lst' and its length
-- Note: you should compute both results with one single recursion.
reverseLength :: [a] -> (Integer, [a])
reverseLength [] = (0, [])
reverseLength (x:xs) = (number + 1, rest ++ [x])
  where (number, rest) = reverseLength xs

-- ***
-- 'isPalindrome lst' is a predicate that checks if 'lst' is a palindrome
-- Last week, you compared 'lst' to its reverse. This does some unneccessary
-- work. In fact, you can save half the work if you stop when you reach the
-- middle of the two lists.
-- Hint: use 'reverseLength' and 'floor'
-- Example:
-- ghci> isPalindrome [1,2,3,2,1]
-- True
-- ghci> isPalindrome [1,2,2,1]
-- True
-- ghci> isPalindrome [1,2,3]
-- False
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) 
  | x == y = isPalindrome zs
  | x /= y = False
  where (_, y:ys) = reverseLength (x:xs)
        (_, _:zs) = reverseLength (ys) 


-- **
-- 'pointwiseMax l1 l2' returns the list of maximum elements in l1 and l2 at each
-- index, stopping at the shorter list of l1 and l2.
-- Example:
-- ghci> pointwiseMax [1,10,5,6] [2,3,7,4,8]
-- [2,10,7,6]
pointwiseMax :: (Ord a) =>[a] -> [a] -> [a]
pointwiseMax _ [] = []
pointwiseMax [] _ = []
pointwiseMax (x:xs) (y:ys)
  |x >= y = x : pointwiseMax xs ys
  |otherwise = y : pointwiseMax xs ys

-- ****
-- 'secondLargest l' returns the second largest element of l.
-- l has to contain at least two distinct elements!
-- Hint: Use an auxiliary function for the recursion.
-- Hint2: The auxiliary function will look exactly like 'foldl'.
-- Example:
-- ghci> secondLargest [1,10,5,6]
-- 6
largest :: (Ord a, Eq a) => [a] -> a
largest [x] = x
largest (x:xs) 
  |x >= largest xs = x
  |otherwise = largest xs


secondLargest :: (Ord a, Eq a) => [a] -> a
secondLargest xs = largest (remove (largest xs) xs)

-- **
-- rewrite secondLargest without recursion but instead using 'foldl'
--secondLargest = undefined