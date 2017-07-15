--redki polinomi
vrednost:: (Fractional a) => [(Integer, a)] -> a -> a
vrednost [] a = 0
vrednost ((n,a):xs) x = a * x ^ n + vrednost xs x 

vsota:: (Fractional a, Ord a, Eq a) => [(Integer, a)] -> [(Integer, a)] -> [(Integer, a)]
vsota [] xs = xs
vsota xs [] = xs
vsota ((n1, a1):xs) ((n2, a2):ys)
	|n1 == n2 && a1 + a2 == 0 = vsota xs ys
	|n1 == n2 = (n1, a1 + a2) : vsota xs ys
	|n1 < n2 = (n1, a1) : vsota xs ((n2, a2):ys) 
	|otherwise = (n2, a2) : vsota ((n1, a1):xs) ys

pomnoziKoef ::(Fractional a, Ord a, Eq a) => (Integer, a) -> [(Integer, a)] -> [(Integer, a)]
pomnoziKoef _ [] = []
pomnoziKoef (n1, a1) ((n2, a2):xs) = (n1 + n2, a1 * a2) : pomnoziKoef (n1, a1) xs


produkt::(Fractional a, Ord a, Eq a) => [(Integer, a)] -> [(Integer, a)] -> [(Integer, a)]
produkt [] xs = []
produkt xs [] = []
produkt ((n1, a1):xs) ys = pomnoziKoef (n1, a1) ys `vsota` produkt xs ys


odstej::[(Integer, Integer)] -> Integer -> [(Integer, Integer)]
odstej [] _ = []
odstej ((n1, a1):xs) n = (n1 - n, a1) : (odstej xs n)


koeficjenti:: [(Integer, Integer)] -> [Integer]
koeficjenti [] = []
koeficjenti ((n1, a1):xs)
	|n1 == 0 = a1: koeficjenti (odstej xs 1)
	|otherwise = 0 : (koeficjenti (odstej ((n1, a1):xs) 1))


