data Drevo a = L a 
				| R (Drevo a) (Drevo a)
				deriving Show

naloga3a :: Drevo a -> [a]
naloga3a (L a) = [a]
naloga3a (R l d) = (naloga3a l) ++ (naloga3a d)

pomozna :: Drevo a -> a -> Drevo a
pomozna (L _) x = (L x) 
pomozna (R l d) x = R (pomozna l x) (pomozna d x)

pomoznaMax:: (Ord a) =>Drevo a -> a
pomoznaMax d = maximum $ naloga3a d 

naloga3b :: (Ord a) => Drevo a -> Drevo a
naloga3b d = (pomozna d) $ pomoznaMax d



naloga3c' :: Drevo a -> [b] -> (Drevo b, [b])

naloga3c' (L _) (x:lx) = (L x, lx)
naloga3c' (L _) [] = error "seznam je prekratek"

naloga3c' (R levo desno) lx = (R novoLevo novoDesno, lx3)
	where
        (novoLevo, lx2) = naloga3c' levo lx
        (novoDesno, lx3) = naloga3c' desno lx2
    
naloga3c :: Drevo a -> [b] -> Drevo b
naloga3c d l =
    let
        (novo, ostanek) = naloga3c' d l
    in
        if length ostanek == 0 then novo else error "seznam je predolg"
