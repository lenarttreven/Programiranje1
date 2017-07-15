
orbita':: (Eq a) =>(a -> a) -> a -> [a] -> [a]
orbita' f x a
	|x `elem` a = a
	|otherwise = orbita' f (f x) (a ++ [x])
 
 
orbita:: (Eq a) =>(a -> a) -> a -> [a]
orbita f x  = (orbita' f x [])


dodajElement:: (Eq a) => (a -> a) -> a -> [([a],[a])] -> [([a],[a])]
dodajElement f a [] = []
dodajElement f a ((as, ys):xs) = (a:as, ys ++ (orbita f a)) : (dodajElement f a xs)


vsi_generiranci::(Eq a) => (a -> a) -> [a] -> [([a],[a])]
vsi_generiranci f [] = []
vsi_generiranci f [x] = [([x], orbita f x)]
vsi_generiranci f (x:xs) = [([x], (orbita f x))] ++ (vsi_generiranci  f xs)  ++ (dodajElement f x (vsi_generiranci  f xs))

podmnozica:: (Eq a) => [a] -> [a] -> Bool
podmnozica [] xs = True
podmnozica (x:xs) ys
	| x `elem` ys = podmnozica xs ys
	|otherwise = False

precisti :: (Eq a) => [a] -> [([a],[a])] -> [([a],[a])]
precisti a [] =[]
precisti a ((x,y):xs)
	|podmnozica a y = (x, y):(precisti a xs)
	|otherwise = precisti a xs

najmanjsi::(Eq a) => [([a],[a])] -> [a]
najmanjsi [(x,y)] = x
najmanjsi ((x1,y1):(x2,y2):xs)
	|(length x1) <= (length x2) = najmanjsi ((x1, y1):xs)
	|otherwise = najmanjsi ((x2,y2):xs)

generatorji::(Eq a) => (a -> a) -> [a] -> [a]
generatorji f xs = najmanjsi $ precisti xs $ vsi_generiranci f xs













