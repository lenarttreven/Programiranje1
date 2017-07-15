data Izraz = Samo Integer | Plus Izraz Izraz | Krat Izraz Izraz deriving Show

izracunaj:: Izraz -> Integer
izracunaj (Samo n) = n
izracunaj (Plus a b) = (izracunaj a) + (izracunaj b)
izracunaj (Krat a b) = (izracunaj a) * (izracunaj b)


instance Num Izraz where
	a + b = (Plus a b) 
	a * b = (Krat a b)
	negate = undefined
	abs = undefined
	signum = undefined
	fromInteger n = (Samo n)