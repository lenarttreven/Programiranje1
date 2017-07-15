{-
 - Exercise set 3: Datatypes
 -}

-- Natural numbers
-- ===============

data Natural = Zero | Succ Natural deriving (Show)

-- 'add m n' returns the sum of natural numbers 'm' and 'n'

add :: Natural -> Natural -> Natural
add Zero num = num
add (Succ num1) num2 = add num1 (Succ num2)

-- 'multiply m n' returns the product of natural numbers 'm' and 'n'

multiply :: Natural -> Natural -> Natural
multiply (Succ Zero) num = num
multiply (Succ num1) num2 = add num2 (multiply num1 num2)

-- 'toNatural n' converts an integer 'n' into a natural number
-- 
-- Example:
-- ghci> toNatural 0
-- Zero
-- ghci> toNatural 2
-- Succ (Succ Zero)

toNatural :: Integer -> Natural
toNatural 0 = Zero
toNatural n = add (Succ Zero) (toNatural (n-1))

-- 'fromNatural n' converts a natural number 'n' to an integer
-- 
-- Example:
-- ghci> fromNatural Zero
-- 0
-- ghci> fromNatural (Succ (Succ Zero))
-- 2

fromNatural :: Natural -> Integer
fromNatural Zero = 0
fromNatural (Succ num1) = 1 + fromNatural num1



-- Trees
-- =====

-- Here we define the recursive datatype Tree. We will add more functions that
-- work on trees, like the sumTree example, which calculates the sum of the
-- elements of a tree.

data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Show)

sumTree :: Num a => Tree a -> a
sumTree Leaf = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

-- 'depth tr' returns the depth of a tree. Leaves have depth 0.
--
-- Example:
-- ghci> let d = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> globina d
-- 3

depth :: (Ord a, Num a) => Tree a -> a
depth Leaf = 0
depth (Node a left right) = 1 + max(depth left) (depth right)


-- 'numberOfElements tr', for 'tr' of type 'Tree alpha' computes the number of
-- alpha's ("elements") in tr.
--
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> numberOfElements tr
-- 4

numberOfElements :: Num a => Tree a -> a
numberOfElements Leaf = 0
numberOfElements (Node a left right) = 1 + numberOfElements left + numberOfElements right

-- 'flip tr' swaps the left and right subtrees of each node in 'tr'
--
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> flip tr
-- Node 3 (Node 8 Leaf Leaf) (Node 7 (Node 2 Leaf Leaf) Leaf)

flip1 :: Num a => Tree a -> Tree a
flip1 Leaf = Leaf
flip1 (Node a left right) = Node a (flip1 right) (flip1 left)

-- 'leftMost tr' returns the left-most element in 'tr'.
-- Example:
-- ghci> let tr = Node 3 (Node 7 Leaf (Node 2 Leaf Leaf)) (Node 8 Leaf Leaf)
-- ghci> leftMost tr
-- 7

leftMost :: Num a => Tree a -> a
leftMost (Node a Leaf _) = a
leftMost (Node a left _) = leftMost left



-- Complex Numbers
-- ==================

-- We have defined a datatype Complex, which represents complex numbers. We
-- will add some functions to work with complex numbers.

data Complex = Complex Double Double deriving (Show)

-- 're x' returns the real part of the complex number x.
re :: Complex -> Double
re (Complex real imag) = real

-- 'im x' returns the imaginary part of the complex number x.
im :: Complex -> Double
im (Complex _ imag) = imag

-- 'conjugate x' returns the complex conjugate of x.
conjugate :: Complex -> Complex
conjugate (Complex real imag) = Complex real (-imag)


-- Polynomials{}
-- ===========

-- We define a datatype Polynomial, which represents polynomials in one
-- variable over the field of rationals, with coefficients in increasing order.
-- We will add more functions to work with polynomials.

data Polinom = Polinom [Rational] deriving (Show)

x :: Polinom
x = Polinom [0, 1]


polinom :: [Rational] -> Polinom
polinom = Polinom . reverse . dropWhile (== 0) . reverse

koeficienti :: Polinom -> [Rational]
koeficienti (Polinom koef) = koef

eval :: Polinom -> Rational -> Rational
eval (Polinom koef) x = sum $ zipWith (\k n -> k * x^n) koef [0..]

-- Sestavite funkcijo, ki izračuna polinom v dani točki. 
--
-- Zgled:
-- ghci> let p = Polinom [2,0,-1]
-- ghci> eval p 2
-- -2

-- Sestavite funkcijo, ki izračuna odvod polinoma (v točki x).

odvod :: Polinom -> Polinom
odvod (Polinom []) = polinom []
odvod (Polinom (_:koef)) = polinom $ zipWith (*) koef [1..]

-- Sestavite funkcijo, ki izračuna nedoločeni integral polinoma.

integral :: Polinom -> Polinom
integral (Polinom koef) = polinom $ 0 : zipWith (/) koef [1..]





