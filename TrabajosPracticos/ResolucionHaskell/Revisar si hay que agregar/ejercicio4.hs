--[] {}
--A
suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

--B
alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs)
      | x == True = True
      | otherwise = alguno xs



--D
codigos :: [Char] -> [Int]
codigos [] = []
codigos (x:xs) = (fromEnum x) : codigos xs

--E
restost :: [Int] -> Int -> [Int]
restost [] n = []
restost (x:xs) n = (x `mod` n): restost xs n

--F
incrementar :: [Int] -> [Int]
incrementar [] = []
incrementar (x:xs) = (x+1): incrementar xs

--G
cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = (x*x): cuadrados xs

--H
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = (length x):longitudes xs

--I
orden :: [(Int,Int)] -> [(Int,Int)]
orden [] = []
orden ((x,y):ls)
      | (x<3*y) = (x,y):orden ls
      | otherwise = orden ls
      
--J
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs)
     | (x `mod` 2) == 0 = x:pares xs
     |otherwise = pares xs
     
--K
letras :: [Char] -> [Char]
letras [] = []
letras (x:xs)
     |64 < (fromEnum x) && 91 > (fromEnum x) = x:letras xs
     |96 < (fromEnum x) && 123 > (fromEnum x) = x:letras xs
     |otherwise = letras xs

--L
masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (x:xs) n
      |(length x) > n = x:(masDe xs n)
      |otherwise = (masDe xs n)

--M
funciones :: [Int -> Int] -> Int -> [Int]
funciones [] _ = []
funciones (f:f2) n = (f n):(funciones f2 n)

misFunciones :: [Int -> Int]
misFunciones = [(*2), (+5)]
     
--Ñ
findList :: Int -> [Int] -> Int
findList n [] = error "No se encuentra ese elemento"
findList n (l:ls) 
      |n /= l = 1 + findList n ls
      |otherwise = 0


