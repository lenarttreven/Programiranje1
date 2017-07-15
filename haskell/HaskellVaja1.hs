dolzina :: (Num b) =>[a] -> b
dolzina [] = 0
dolzina (_:xs) = 1 + dolzina xs

vsota :: (Num a) => [a] -> a
vsota [] = 0
vsota (a:b) = a + vsota b

capital :: String -> String
capital [] = "??"
capital all@(x:xs) = "Prva crka od " ++ all ++ "je " ++ [x]


najvecji :: (Ord a) => a -> a -> a
najvecji a b 
	| a >= b = a
	| otherwise = b

primerjaj :: (Ord a) => a -> a -> Ordering
primerjaj a b
	| a > b  = GT
	| a == b = EQ
	| a < b = LT


calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2 