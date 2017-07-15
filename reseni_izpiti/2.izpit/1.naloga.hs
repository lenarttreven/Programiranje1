data Drevo a = Drevo a [Drevo a] deriving Show


vsota:: (Num a) => (Drevo a) -> a
vsota (Drevo a []) = a
vsota (Drevo a xs) = a + sum (map vsota xs)


najvecji :: (Ord a) => [(Drevo a)] -> a
najvecji [(Drevo a _)] = a
najvecji ((Drevo a _):xs) = max a (najvecji xs)

jeKopica::(Ord a) => (Drevo a) -> Bool
jeKopica (Drevo a []) = True
jeKopica (Drevo a xs)
	|(a >= najvecji xs) && (and (map jeKopica xs)) = True
	|otherwise = False

najvecjaPodkopica ::(Ord a) => [(Drevo a)] -> (Drevo a)
najvecjaPodkopica [(Drevo a xs)] = (Drevo a xs)
najvecjaPodkopica ((Drevo a xs):ys) 
	|a >= najvecji ys = (Drevo a xs)
	|otherwise = najvecjaPodkopica ys


odstraniKopico:: (Ord a) => [(Drevo a)] -> (Drevo a) -> [(Drevo a)]
odstraniKopico [] x = []
odstraniKopico ((Drevo a as):xs) (Drevo y ys) 
	|a == y = xs
	|otherwise = (Drevo a as) : (odstraniKopico xs (Drevo y ys))


odstraniMax:: (Ord a) => (Drevo a) -> (a, Maybe (Drevo a))
odstraniMax (Drevo a []) = (a, Nothing)
odstraniMax (Drevo a xs) = (a, Just (Drevo (najvecji xs) novaKopica))
	where novaKopica = (novaKopica' ++ (odstraniKopico xs (najvecjaPodkopica xs)))   
		  where (Drevo c novaKopica') = (najvecjaPodkopica xs) 
			
	
padajociElementi:: (Ord a) => (Drevo a) -> [a]
padajociElementi a = 
	case odstraniMax a of 
		(x, Nothing) -> [x]
		(x, Just xs) -> x : (padajociElementi xs)


