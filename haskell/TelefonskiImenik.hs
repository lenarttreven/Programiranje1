import Slovar

-- Tukaj bomo sestavili preprost telefonski imenik. To bo slovar, ki bo pri
-- vsakem imenu imel vrednost. Telefonske številke so lahko precej različne,
-- zato bomo za te vrednosti uporabili tip String.

type Imenik = Slovar String String

-- Uporabniku bodo na voljo naslednje operacije za delo z imenikom:
data Operacija = Dodaj | Poisci | Odstrani | IzpisiVse

-- Definirajmo še tip Ukaz, ki je lahko znana operacija, nekaj nepoznanega ali 
-- poseben ukaz Izhod.
data Ukaz = Op Operacija | NepoznanUkaz String | Izhod

-- Funkcija [preberiIme] prosi uporabnika za vnos imena in ga prebere iz ukazne vrstice.
preberiIme :: IO String
preberiIme = do
  putStr "Vnesi ime: "
  getLine

-- Implementiraj še funkcijo [preberiStevilko], ki prosi uporabnika za vnos telefonske
-- stevilke in jo prebere.
preberiStevilko :: IO String
preberiStevilko = do
	putStr "Vnesi stevilko: "
	getLine

-- Implementiraj funkcijo [preberiUkaz], ki prosi uporabnika, da vnese enega od
-- ukazov, ki so na voljo, in potem vrne [Ukaz], ki ga je uporabnik vnesel.
preberiUkaz :: IO Ukaz
preberiUkaz = do
	putStr "Izberi ukaz (Dodaj, Poisci, Odstrani, IzpisiVse)"
	cmd <- getLine
	case cmd of 
		"Dodaj" -> return (Op Dodaj)
		"Poisci" -> return (Op Poisci)
		"Odstrani" -> return (Op Odstrani)
		"IzpisiVse" -> return (Op IzpisiVse)
		"Izhod" -> return Izhod
		_ -> return (NepoznanUkaz cmd)


-- [izvediOperacijo imenik op] izvede operacijo [op] na imeniku [imenik].
izvediOperacijo :: Imenik -> Operacija -> IO Imenik

izvediOperacijo imenik IzpisiVse = do
  putStrLn $ show imenik
  return imenik

izvediOperacijo imenik Poisci = do
	ime <- getLine
	case (poisci ime imenik) of 
		Just a -> putStrLn $ show a
		Nothing -> putStrLn "Imena ni v imeniku"
	return imenik

izvediOperacijo imenik Dodaj = do
	ime <- preberiIme
	stevilka <- preberiStevilko
	return $ dodaj ime stevilka imenik
	

izvediOperacijo imenik Odstrani = do
	ime <- preberiIme
	return $ odstrani ime imenik

-- [interakcijskaZanka imenik] prebere [Ukaz] in izvede naslednje: 
-- * če je ukaz operacija, jo izvede
-- * če je ukaz nepoznan, obvesti uporabnika o tem, da ukaza ne pozna
-- * če je ukaz [Izhod], se funkcija prekine, torej izvede se `return ()`.

interakcijskaZanka :: Imenik -> IO ()
interakcijskaZanka imenik = do
	cmd <- preberiUkaz
	case cmd of 
		Op op -> do 
			nov_imenik <- izvediOperacijo imenik op
			interakcijskaZanka nov_imenik
		NepoznanUkaz str -> do 
			putStrLn("Nepoznan ukaz")
			interakcijskaZanka imenik
		Izhod -> return ()

main :: IO ()
main = interakcijskaZanka prazen

-- Nazadnje ta program prevedi v strojno kodo in sestavi .exe datoteko.

--runghc phonebook.hs
