{-# LANGUAGE MultiParamTypeClasses #-}

import Data.List (sort, maximumBy, sortBy)
import Data.Function (on)

-- Complex Numbers
-- ===============
--
-- Show that the types Natural, Complex and Polynomial of the previous lesson
-- belong to the Num class:

data Natural = Zero | Succ Natural deriving (Show)

instance Num Natural where
  Zero + n = n
  (Succ m) + n = Succ (m + n)
  
  Zero * n = Zero
  (Succ n) * m = (n * m) + m

  fromInteger 0 = Zero
  fromInteger n = Succ Zero + fromInteger (n-1)

  abs n = n

  signum Zero = 0
  signum n = 1

  negate = error "Operation doesn't make any sense"

data Complex = Complex Double Double deriving (Show)

instance Num Complex where
  (Complex x1 y1) + (Complex x2 y2) = Complex (x1 + x2) (y1 + y2)

  (Complex x1 y1) * (Complex x2 y2) = Complex (x1 * x2 - y1 * y2) (x1 * y2 + y1 * x2)

  abs (Complex x1 y1) = Complex (sqrt (x1 ^ 2 + y1 ^ 2)) 0

  (Complex x1 y1) - (Complex x2 y2) = Complex (x1 - x2) (y1 - y2)
  
  fromInteger n = Complex  (fromInteger n) 0

  signum (Complex x1 y1) = Complex (x1/ (sqrt (x1 ^ 2 + y1 ^ 2))) (y1/ (sqrt (x1 ^ 2 + y1 ^ 2)))

data Polinom = Polinom [Rational] deriving (Show)

polinom :: [Rational] -> Polinom
polinom = Polinom . reverse . dropWhile (== 0) . reverse

koeficienti :: Polinom -> [Rational]
koeficienti (Polinom koef) = koef

instance Num Polinom where
    negate (Polinom koef) = polinom $ pomnoziKoef (-1) koef
    (Polinom koef1) + (Polinom koef2) = polinom $ sestejKoef koef1 koef2
    (Polinom koef1) * (Polinom koef2) = polinom $ zmnoziKoef koef1 koef2
    fromInteger x = polinom $ [fromInteger x]
    abs = error "abs: polinomi nimajo definirane absolutne vrednosti"
    signum = error "abs: polinomi nimajo definiranega predznaka"
 
sestejKoef :: [Rational] -> [Rational] -> [Rational]
sestejKoef xs [] = xs
sestejKoef [] ys = ys
sestejKoef (x:xs) (y:ys) = (x + y):(sestejKoef xs ys)


pomnoziKoef :: Rational -> [Rational] -> [Rational]
pomnoziKoef a = map (a *)

zmnoziKoef :: [Rational] -> [Rational] -> [Rational]
zmnoziKoef _ [] = []
zmnoziKoef [] _ = []
zmnoziKoef (x:xs) (y:ys) = (x * y) : pomnoziKoef x ys `sestejKoef` (xs `zmnoziKoef` (y:ys))


-- Algebraic structures
-- ====================

-- The [Semigroup] class can be defined like this:

class  Semigroup a  where
    (***) :: a -> a -> a

-- Define the following classes (extensions):
-- + SemigroupWithUnit (with a special element "unit")
-- + Group (with an "inv" function)
-- + Ring

class Semigroup a => SemigroupWithUnit a where
    e :: a

class SemigroupWithUnit a => Group a where
  inv :: a -> a 

class Ring a where 
  zero :: a
  unit :: a
  (****) :: a -> a -> a
  (++++) :: a -> a -> a
  addinv :: a -> a

-- Show that the integers belong to the Ring class

instance Ring Integer where
   zero = 0
   unit = 1
   m **** n = m * n
   m ++++ n = m + n
   addinv m = -m


-- Show that Bool belongs to Group
instance Semigroup Bool where
  m *** n = m && n

instance SemigroupWithUnit Bool where
  e = True

instance Group Bool where
  inv True = True
  inv False = False

-- Show that the type [Z_2] as defined below belongs to Group

data Z_2 =  Zero_2 | One_2 deriving (Show)

instance Semigroup Z_2 where
    Zero_2 *** Zero_2 = Zero_2
    Zero_2 *** One_2 = One_2
    One_2 *** Zero_2 = One_2
    One_2 *** One_2 = Zero_2

instance  SemigroupWithUnit Z_2 where
    e = Zero_2

instance Group Z_2 where
  inv Zero_2 = Zero_2
  inv One_2 = One_2
-- Show that the cartesian product type of two types in the Group class belongs
-- to the Group class


instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
    (x1, y1) *** (x2, y2) = (x1 *** x2, y1 *** y2)

instance (SemigroupWithUnit a, SemigroupWithUnit b) => SemigroupWithUnit (a, b) where
  e = (e, e)

instance (Group a, Group b) => Group (a, b) where
  inv (a, b)  = (inv a, inv b)

-- Let types [a] and [b] belong to the Group class. To say that [a] and [b] are
-- isomorphic, we can define the Isomorphism class:

class  Isomorphism a b  where
    towards :: a -> b
    backwards :: b -> a


instance Isomorphism Bool Z_2 where
  towards True = Zero_2
  towards False = One_2
  backwards Zero_2 = True
  backwards One_2 = False
-- Show that [Bool] and [Z_2] are isomorphic as groups

-- Distributions
-- =============

-- We define the data type of distributions:

data Distribution a = Distribution [(a, Rational)] deriving Show

-- Two simple examples:

data Coin = Heads | Tails
coin :: Distribution Coin
coin = Distribution [(Heads, 1/2), (Tails, 1/2)]
die :: Distribution Int
die = Distribution [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]

-- [isDistribution d] checks that d really is a distribution, ie that the
-- probabilities sum up to 1.

isDistribution :: Distribution a -> Bool
isDistribution (Distribution xs) = sum (map snd xs) == 1

 
-- [cleanDistribution d] merges together repeated events
cleanDistribution :: Ord a => Distribution a -> Distribution a
cleanDistribution (Distribution xs) = 
	Distribution $ merge $ sortBy (compare `on` fst) xs
	where
		merge ((x1, y1):(x2, y2):ys)
			|x1 == x2 = merge ((x1, y1 + y2):ys)
			|otherwise = (x1, y1) : merge ((x2, y2):ys)
		merge ys = ys

-- [mostLikely d] returns the most likely event in [d], and the last one listed
-- if it is not unique.

mostLikely :: Distribution a -> a
mostLikely (Distribution [(x,p)]) = x 
mostLikely (Distribution ((x1, y1):(x2, y2):xs))
	|y1 > y2 = mostLikely (Distribution ((x1, y1):xs))
	|y1 <= y2 = mostLikely (Distribution ((x2, y2):xs))


-- [uniform d] returns a uniform distribution of the events in d

uniform :: [a] -> Distribution a
uniform xs = Distribution b
    where b = [(x, 1 / (fromIntegral (length xs)))| x <- xs]

-- [expectation d] returns the expected value of d

expectation :: (a -> Rational) -> Distribution a -> Rational
expectation f (Distribution a) = sum[y * f x |(x, y) <- a]

-- [weightedSum p d1 d2] computes a distribution obtained by merging the
-- (compatible) distributions [d1] and [d2], scaling events in [d1] by
-- the weight [p] and events in [d2] by (1-p).

weightedSum :: Ord a => Rational -> Distribution a -> Distribution a -> Distribution a
weightedSum p (Distribution a) (Distribution b) = 
	cleanDistribution $ Distribution $ ((factor p a) ++ (factor (1-p) b))
		where factor p xs = [(x, p * y)|(x, y) <- xs] 


-- Show that the type constructor [Distribution] belongs to the [Functor] type
-- class.

instance  Functor Distribution  where
    fmap f (Distribution a) = Distribution [(f x, y)|(x, y)<- a]



-- Moving in space
-- ===============

-- Define a [Point] type, which should have the parameters (name, X-coordinate,
-- Y-coordinate, Z-coordinate). Implement a [show] function for [Point]

data Point = Point (Double, Double, Double) deriving (Show)


-- The point will move through space. Before we can move, we must first be
-- somewhere. Define a type class [Positioned] with a [currentPosition] value
-- and show that [Point] is a member of [Positioned]

class Positioned a where
	currentPosition :: a -> (Double, Double, Double)

instance  Positioned Point where
	currentPosition (Point (x, y, z)) = (x, y, z) 

-- Define the [Movable] type class that implements [setNewLocation] and make
-- [Point] a member of it.

class Movable a where
	setNewLocation:: a ->(Double, Double, Double)  -> a
	moveFor:: a -> (Double, Double, Double) -> a

instance Movable Point where
	setNewLocation (Point a) (x, y, z) = Point (x, y, z)
	moveFor (Point (x, y, z)) (a, b, c) = Point (x + a, y + b, z + c) 

-- Za spremenljivke, katerih tip pripada razredu Premakljiv, definirajte
-- funkcijo premakniZa, ki spremenljivko premakne za določen vektor. Ta funkcija
-- bo tako delovala tudi za poljubno točko!

--class Movable a where
--	moveFor:: a -> (Rational, Rational, Rational) -> a
-- Kako dodati funkcijo v class tako da to dodaš na mestu in ne kjer je class prvotno definiran

-- In some species of spiders, the female is known to eat the male after
-- mating. The females which eat the males lay more eggs, which produce
-- stronger and bigger embryos. One tentative explanation is that such females
-- are more aggressive and therefore better hunters. Another theory says that
-- males simply are an excellent source of essential nutrients.
--
-- Imagine a female spider at location (0,0,0). The female wants to eat the
-- male which is at coordinates (3,3,3). Assume that the spiders' movements are
-- confined to a 10x10x10 cube (ie each coordinate is always between 0 and 9).
--
-- We will make both spiders move around. First moves the female, which tries
-- to get as close as possible to the male's position. Then moves the male,
-- which wants to maximize the distance from the female.

-- Write a [move f m] function that maps a position of a female [f] and of a
-- male [m] to a pair of moves.

distance :: Point -> Point -> Double 
distance (Point (x1, y1, z1)) (Point (x2, y2, z2)) = sqrt ((x1-x2)^2+(y1-y2)^2+(z1-z2)^2) 

min_distance:: [Point] -> Point -> Point
min_distance [(Point x)] (Point a) = Point x
min_distance ((Point x) :(Point y):xs) (Point a)
	| distance (Point x) (Point a) <= distance (Point y) (Point a) = min_distance ((Point x): xs) (Point a)
	| otherwise = min_distance ((Point y):xs) (Point a)

max_distance:: [Point] -> Point -> Point
max_distance [(Point x)] (Point a) = Point x
max_distance ((Point x) :(Point y):xs) (Point a)
	| distance (Point x) (Point a) >= distance (Point y) (Point a) = max_distance ((Point x): xs) (Point a)
	| otherwise = max_distance ((Point y):xs) (Point a)

candidatePoints:: Point -> [Point]
candidatePoints (Point (x, y, z)) = [Point (a, b, c)|(a, b, c) <- [(x+1, y, z),(x-1, y, z),(x, y+1, z),(x, y-1, z),(x, y, z+1),(x, y, z-1)], 
									  a >= - 9, a <= 9, b >= - 9, b <= 9, c >= - 9, c <= 9]
                 
moveFemale:: Point -> Point -> Point
moveFemale (Point x) (Point a) = y
	where y = min_distance (candidatePoints (Point x)) (Point a)

moveMale:: Point-> Point -> Point
moveMale (Point x) (Point a)
	 |x == a = Point x 
	 |otherwise = max_distance (candidatePoints (Point x)) (Point a)


move::(Point, Point) -> (Point, Point) -- (female, male)
move ((Point a),(Point b)) = (moveFemale (Point a) (Point b), moveMale (Point b) (moveFemale (Point a) (Point b)))
		

-- Finally, write [simulate], which simulates the behaviour of a female and a male.

simulate:: (Point, Point) -> [(Point, Point)]
simulate ((Point a), (Point b))
	|a == b = [((Point a), (Point b))]
	|otherwise = ((Point a), (Point b)):(simulate (move ((Point a), (Point b))))









