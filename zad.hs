import Data.List
import Control.Monad
import qualified Data.Map as Map

-- Zadanie 1

-- 1.
czy_perm :: (Ord a) => [a] -> [a] -> Bool
czy_perm [] [] = False
czy_perm a b = sortuj a == sortuj b

sortuj :: (Ord a) => [a] -> [a]
sortuj [] = []
sortuj (h:t) =
	let	wieksze = sortuj [a | a <- t, a > h]
		mniejsze = sortuj [a | a <- t, a <= h]
	in wieksze ++ [h] ++ mniejsze
	
-- 8.
pascal :: Integer -> [Integer]
pascal n = [newton n x | x <- compute_row n]

compute_row :: Integer -> [Integer]
compute_row n = if n /= 0 then compute_row(n-1) ++ [n] else [0]

power :: Integer -> Integer
power n = if (n == 1 || n==0) then 1 else  n * power(n-1)

newton :: Integer -> Integer -> Integer 
newton n k = div (power(n)) (power(n-k)*(power(k)))

-- 11.
zbior_potegowy :: [a] -> [[a]]
zbior_potegowy list = powieksz [[]] list 

powieksz :: [[a]] -> [a] -> [[a]]
powieksz listalist [] = listalist;
powieksz listalist elementy = powieksz (concat (map (dodaj_i_nie_dodaj (head elementy)) listalist)) (tail elementy)

dodaj_i_nie_dodaj :: a -> [a] -> [[a]]
dodaj_i_nie_dodaj element lista = [ lista, element:lista ]

-- #########################################
-- Zadanie 2

-- funkcja wklejajaca zadany String(str) pomiedzy znaki innego Stringu(base)
insertStr :: String -> String -> String
insertStr "" str = ""
insertStr [a] str = [a]
insertStr (hbase:tbase) str = [hbase] ++ str ++ insertStr tbase str

-- definicje naszych predykatow
data Zdanie = Z Char
				| N Zdanie
				| K Zdanie Zdanie
				| A Zdanie Zdanie
				| C Zdanie Zdanie
	
usun_dup :: (Eq a) => [a] -> [a]
usun_dup [] = []
usun_dup (h:t)
  | znajdz h t = usun_dup t
  | otherwise = h : usun_dup t	
	
znajdz :: (Eq a) => a -> [a] -> Bool
znajdz _ [] = False
znajdz a (h:t) 
  | h == a    = True
  | otherwise = znajdz a t	
		
-- overload funkcji wyswietlajacej obiekt, pierwotnie sluzyla do unikniecia "" przy wyswietlaniu string
-- aktualnie chyba mozna zamienic na dowolna inna funkcje
instance Show Zdanie where
	show (Z c) = [c]
	show (N x) = show x
	show (K x y) = usun_dup( show x ++ show y)
	show (A x y) = usun_dup( show x ++ show y)
	show (C x y) = usun_dup( show x ++ show y)

-- 1.
-- Funkcja zamieniajaca Zdanie na String w wygodnej formie
drukuj :: Zdanie -> String
drukuj (Z x) = [x]
drukuj (N x) = "~" ++ drukuj x
drukuj (K x y) = "(" ++ drukuj x ++ " & " ++ drukuj y ++ ")"
drukuj (A x y) = "(" ++ drukuj x ++ " | " ++ drukuj y ++ ")"
drukuj (C x y) = "(" ++ drukuj x ++ " => " ++ drukuj y ++ ")"

-- 2.
-- Funkcja wypisujaca zmienne, wiekszosc logiki jest w show i insertStr
wypisz_zmienne :: Zdanie -> IO ()
wypisz_zmienne a = putStrLn ( "[" ++ (insertStr (show a) "," ) ++ "]" )

-- 3.
-- Funkcja sprawdzajaca wartoœæ zdania dla podanych parametrów
sprawdz :: Zdanie -> Map.Map Char Bool -> Bool
sprawdz (Z x) map = Map.findWithDefault False x map 
sprawdz (N x) map = not(sprawdz x map)
sprawdz (K x y) map = (sprawdz x map) && (sprawdz y map)
sprawdz (A x y) map = (sprawdz x map) || (sprawdz y map)
sprawdz (C x y) map = not(sprawdz x map) || (sprawdz y map)

-- 4.b
-- Funkcja sprawdza czy Zdanie jest tautologia przez sprawdzenie wszystkich wariacji
jest_tautologia :: Zdanie -> Bool
jest_tautologia z = usun_dup (map (sprawdz z) (wygeneruj_kombinacje z)) == [True]

wygeneruj_kombinacje :: Zdanie -> [Map.Map Char Bool]
wygeneruj_kombinacje z = map (polacz_w_mape (show z)) (replicateM (length (show z)) [True, False]);

polacz_w_mape :: [Char] -> [Bool]  -> Map.Map Char Bool
polacz_w_mape [] [] = Map.empty
polacz_w_mape chars bools = Map.fromList (zip chars bools)

-- Przyk³adowe dane
z = Z 'z'
z2 = Z 'z'
x = Z 'x'
k = K z x
a = A x z2
c = C z z2
taut = A (Z 'p') (N (Z 'p'))
complex = (C (N (Z 'p')) (A (K (Z 'p') (Z 'q')) (Z 'r')))
testMap = Map.fromList [('p', False), ('q', True), ('r', False)]