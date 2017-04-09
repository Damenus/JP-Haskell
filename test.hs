-- tablice ze znakami

setki=["","sto", "dwiescie","trzysta","czterysta","piecset","szescset","siedemset",
      "osiemset","dziewiecset"]

dziesiatki=["","dziesiec","dwadziescia","trzydziesci","czterdziesci", "piecdziesiat",
  "szescdziesiat", "siedemdziesiat", "osiemdziesiat", "dziewiecdziesiat"]

jedenastki=["", "jedenascie", "dwanascie","trzynascie","czternascie",
            "pietnascie", "szesnascie", "siedemnascie", "osiemnascie",
              "dziewietnascie"]

jednosci=["","jeden","dwa", "trzy", "cztery","piec","szesc", "siedem","osiem","dziewiec"]

zero = "zero"

liczbaZnakow::Int -> (Int,String) -- Int
liczbaZnakow x
      | x==0 = (length zero,zero)
  --  | length cyfry == 3 = length (trzyc cyfry)
  --  | length cyfry == 2 = length (dwuc cyfry)
  --  | length cyfry == 1 = length (jednc cyfry)
      | length cyfry == 3 = (length (trzyc cyfry),trzyc cyfry)
      | length cyfry == 2 = (length (dwuc cyfry),dwuc cyfry)
      | length cyfry == 1 = (length (jednc cyfry),jednc cyfry)
      | otherwise = (0,"")
    where cyfry = zamienNaCyfry x


-- funkcje skladajace wyrazy w jeden string

--trzycyfrowe
trzyc:: [Int] -> String
trzyc x = let idx = x!!0 in setki!!idx ++ dwuc(tail x)

--dwucyfrowe
dwuc:: [Int] -> String
dwuc x
      | ((idx == 1) && (x!!1 > 0))  = jedeski(tail x) -- przypadek dla liczb 11-19
      | otherwise = dziesiatki!!idx ++ jednc(tail x)
      where idx = x!!0

--jedenastki
jedeski:: [Int] -> String
jedeski x = let idx = x!!0 in jedenastki!!idx

--jednosci
jednc :: [Int] -> String
jednc x = let idx = x!!0 in jednosci!!idx

--zamiana liczby na listę cyfr
zamienNaCyfry :: Int -> [Int]
zamienNaCyfry x
        | x==0 = []  --dla 0 zwroc pusta liste
        |otherwise = zamienNaCyfry (div x 10) ++ [mod x 10]
        -- div bo chcemy bez reszty z dzielenia
        -- wynik dzielenia przekaz dalej, reszte dopisz do listy z przodu
