-- Algorytm Euklidesa (najwiekszy wspolny dzielnik)
-- www.algorytm.org

my_gcd :: Int -> Int -> Int
my_gcd a b
	| b == 0    = a
	| otherwise = my_gcd b (a `mod` b)
