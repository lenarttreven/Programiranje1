data Drevo = Drevo String [Drevo] deriving Show


element:: String -> Drevo -> Bool
element ime (Drevo d []) 
	|d == ime = True
	|otherwise = False
element ime (Drevo d xs)
	|d == ime = True
	|otherwise = or (map (element ime) xs)


veriga:: Drevo -> String -> [String]
veriga (Drevo a d) ime 
	|a == ime = [ime]  
	|element ime (Drevo a d) = map (\x -> veriga x ime) d ++ [a]
	|otherwise = "nenaredinic"