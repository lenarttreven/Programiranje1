data Drevo a = Drevo a [Drevo a] deriving Show


vsota :: (Num a) => Drevo a -> a
vsota (Drevo x []) = x
vsota (Drevo a xs) = a + sum (map vsota xs)

jeKopica :: (Ord a, Num a) => Drevo a -> Bool
jeKopica (Drevo x []) = True
jeKopica (Drevo a ((Drevo x ys):xs)) 
	|a < x = False
	|otherwise =  (jeKopica (Drevo x ys)) && (jeKopica (Drevo a xs))

--najvecji:: (Ord a) => [Drevo a] -> a
--najvecji [Drevo x []] = x
--najvecji ((Drevo y ys):zs) = max y (najvecji zs)

odstraniMax :: Ord a => Drevo a -> (a, Maybe (Drevo a))
odstraniMax (Drevo x []) = (x, Nothing)
odstraniMax (Drevo x ds) = (x, Just (Drevo x' (ds' ++ ds'')))
  where
    (Drevo x' ds', ds'') = odstraniNajvecjegaOtroka ds
    odstraniNajvecjegaOtroka [Drevo x ds] = (Drevo x ds, [])
    odstraniNajvecjegaOtroka (Drevo x ds : ds') =
        let (Drevo x'' ds'', ds''') = odstraniNajvecjegaOtroka ds' in
        if x < x'' then
            (Drevo x'' ds'', Drevo x ds : ds''')
        else
            (Drevo x ds, ds')

padajociElementi:: Ord a => Drevo a -> [a]
padajociElementi xs = 
	case odstraniMax xs of
		(y, Nothing) -> [y]
		(y, Just ys) -> y : padajociElementi ys


--(y, Nothing) == odstraniMax xs = [y]
--(y, Just ys) == (odstraniMax xs) = y : (padajociElementi ys)





--najvecjeDrevo:: (Ord a) => [Drevo a] -> Drevo a 
--najvecjeDrevo (x:[])  = x
--najvecjeDrevo a@((Drevo x xs):ys)
--	| x == najvecji a = (Drevo x xs)
--	|otherwise = najvecjeDrevo ys
--
--poddrevo :: Drevo a -> [Drevo a]
--poddrevo (Drevo x xs) = xs
--
--odstraniMax :: (Ord a) => Drevo a -> (a, Maybe (Drevo a))
--odstraniMax (Drevo x []) = (x, Nothing)
--odstraniMax (Drevo x xs) = (x, Maybe a)
--	where a = Drevo y ys
--		  y = najvecji xs
--		  ys = poddrevo ++ xs
--		   
