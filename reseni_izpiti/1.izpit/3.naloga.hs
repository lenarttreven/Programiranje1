--Drevesa

data Drevo a = List a | Razvejano (Drevo a) (Drevo a) deriving Show

naloga3a:: (Drevo a) -> [a]
naloga3a (List a) = [a]
naloga3a (Razvejano a b) = (naloga3a a) ++ (naloga3a b)


najvecji::(Ord a) => (Drevo a) -> a
najvecji (List a) = a
najvecji (Razvejano x y) = max (najvecji x) (najvecji y)

zamenjaj:: (Drevo a) -> a -> (Drevo a)
zamenjaj (List a) x = (List x)
zamenjaj (Razvejano a b) x = (Razvejano (zamenjaj a x) (zamenjaj b x))

naloga3b :: (Ord a) => (Drevo a) -> (Drevo a)
naloga3b a = zamenjaj a (najvecji a)

elementi :: (Drevo a) -> Int
elementi (List a) = 1
elementi (Razvejano a b) = (elementi a) + (elementi b)

naloga3c :: Drevo a -> [b] -> Drevo b
naloga3c (List a) [x] = (List x)
naloga3c (Razvejano a b) xs = (Razvejano (naloga3c a (take (elementi a) xs )) (naloga3c b (drop (elementi a) xs))) 
naloga3c (List a) [] = error "seznam je prekratek"
naloga3c (List _) xs = error "seznam je predolg"


