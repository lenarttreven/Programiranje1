naloga1a:: (Num a) => [(a ->a)]-> a -> a
naloga1a [] a = a
naloga1a (f:fs) a = naloga1a fs (f a)


naloga1b::(Num a, Ord a) => [(a ->a)]-> a -> a
naloga1b [] a = a
naloga1b (f:fs) a
	|f a >= a = naloga1b fs (f a)
	|otherwise = naloga1b fs a 

naloga1c:: (Num a, Ord a) => [(a ->a)]-> a -> a
naloga1c [] a = a
naloga1c (f:fs) a
	|(naloga1c fs (f a)) >= (naloga1c fs a) = naloga1c fs (f a)
	|otherwise = naloga1c fs a

