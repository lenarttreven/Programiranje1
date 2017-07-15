import Data.List

data Drevo = Drevo String [Drevo]

jeElement:: String -> Drevo -> Bool
jeElement x (Drevo a [])
	|a == x = True
	|otherwise = False
jeElement a (Drevo x xs)
	|a == x = True
	|otherwise = (or (map (jeElement a) xs))


veriga:: String -> Drevo ->  [String]
veriga a d@(Drevo x xs) 
	|a == x = [x]
	|jeElement a d = [x] ++ concat (map (veriga a) xs)
	|otherwise = []


veriga' :: String -> Drevo ->  [String]
veriga' x y = reverse (veriga x y)

sestaviList:: Drevo -> [String]
sestaviList (Drevo a []) = [a]
sestaviList (Drevo a xs) = [a] ++ concat (map sestaviList xs)

uredi:: [String] -> [String]
uredi xs = sort xs

ekipa:: String -> Drevo -> [String]
ekipa a (Drevo x [])
	|a == x = [a]
	|otherwise = []
ekipa a d@(Drevo x xs)
	|a == x = uredi $ sestaviList d
	|not (jeElement a d) = []
	|otherwise = concat (map (ekipa a) xs) 


neposredniOdnos:: String -> String -> Drevo -> Bool
neposredniOdnos x y d 
	|length (veriga y d) <= 1 = False
	|length (veriga x d) <= 1 = False
	|(veriga x d) !! 1 == y = True
	|(veriga y d) !! 1 == x = True
	|otherwise = False
	

jeSprosceno:: [String] -> Drevo -> Bool
jeSprosceno [x] _ = True
jeSprosceno (x:y:xs) d
	|neposredniOdnos x y d = False
	|otherwise = (jeSprosceno (x:xs) d) && (jeSprosceno (y:xs) d)


presek :: (Eq a) => [[a]] -> [a]
presek [x] = x 
presek (x:xs) = intersect x (presek xs)


narediSeznam:: [String] -> Drevo -> [[String]]
narediSeznam [x] a = [(veriga x a)]
narediSeznam (x:xs) a = (veriga x a) : (narediSeznam xs a) 

obrekovanje:: [String] -> Drevo -> (Maybe String)
obrekovanje xs d = 
	case presek $ (narediSeznam xs d) of
		[x] -> Just x
		[] -> Nothing




