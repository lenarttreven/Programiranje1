--1. naloga

naloga1a:: (Num a) => [a->a] -> a -> a
naloga1a [] a = a
naloga1a (x:xs) a = naloga1a xs (x a) 

naloga1b:: (Ord a, Num a) => [a->a] -> a -> a
naloga1b [] a = a
naloga1b (x:xs) a 
	|x a > a = naloga1b xs (x a)
	|otherwise = naloga1b xs a

naloga1c:: (Ord a, Num a) => [a->a] -> a -> a
naloga1c [] a = a
naloga1c (x:xs) a = max (naloga1c xs a) (naloga1c xs (x a))