hitroObrni:: [a] -> [a]
hitroObrni  = foldl (\acc x -> x:acc) []

pocasiObrni :: [a] -> [a]
pocasiObrni = foldr (\x acc -> acc ++ [x]) []