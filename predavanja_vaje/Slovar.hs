module Slovar where 
	type Slovar k v = [(k,v)]

	prazen :: Slovar k v
	prazen = []

	dodaj :: (Eq k) =>Slovar k v -> k -> v -> Slovar k v
	dodaj [] k v = [(k,v)]
	dodaj ((k, v):s) k' v' 
		|k == k'= ((k, v'):s)
		|otherwise = (k, v) : dodaj s k' v' 


	poisci :: (Eq k) => Slovar k v -> k -> Maybe v
	poisci [] k' = Nothing
	poisci ((k,v):s) k' 
		|k == k' = Just v
		|otherwise = poisci s k'

	pobrisi :: Slovar k v -> k -> v -> Slovar k v
	pobrisi = undefined
