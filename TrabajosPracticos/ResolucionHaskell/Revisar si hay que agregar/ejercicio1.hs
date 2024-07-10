--A
volumen :: Float -> Float
volumen r = pi * (4/3) * r^3

--B
sumaCoins :: Int -> Int -> Int -> Int -> Int -> Float
sumaCoins a b c d e = fromIntegral(a + b*5 + c*10 + d*50 + e*100)/100

--C
incrementarTupla :: Num a => [a] -> [a]
incrementarTupla xs = [x + 1 | x <- xs]

--D
cuadrado :: Num a => a -> a
cuadrado x = x^2

--E
elevadoALaCuarta :: Num a => a -> a
elevadoALaCuarta x = cuadrado(cuadrado(x))

--F
mediaAritmetica3 :: Floating a => (a,a,a) -> a
mediaAritmetica3 (a,b,c) = (a + b + c)/3 

--G
maximo3 :: (Floating a, Ord a) => a -> a -> a -> a
maximo3 a b c = max a (max b c)

--H
maximo6 :: (Floating a, Ord a) => a -> (a -> (a -> (a -> (a -> (a -> a)))))
maximo6 a b c d e f = max (maximo3 a b c) (maximo3 d e f)

--I
{-
semiperimetro :: Floating a => a -> (a -> (a -> a))
semiperimetro a b c = (a+b+c)/2


area :: Floating a => a -> a -> a -> a
area = 
-}

--J
cuadrante :: (Floating a, Ord a) => (a,a) -> a
cuadrante (a, b) |a>0 && b>0 = 1
		 |a<0 && b>0 = 2
		 |a<0 && b<0 = 3
		 |a>0 && b<0 = 4
		 |otherwise = 0

--K
igualesTres :: Eq a => a -> (a -> (a -> Bool))
igualesTres a b c = a == b && b == c

--L
diferentesTres :: Eq a => a -> (a -> (a -> Bool))
diferentesTres a b c = a /= b && b /= c

--M
igualesCuatro :: Eq a => a -> (a -> (a -> (a -> Bool)))
igualesCuatro a b c d = igualesTres a b c && c == d

--N
raices :: (Floating a, Ord a) => a -> a -> a -> [a]
raices a b c
  | discriminante < 0 = []
  | otherwise = [(-b + sqrt discriminante) / (2*a), (-b - sqrt discriminante) / (2*a)]
  where discriminante = b^2 - 4*a*c
  
--Ñ
bisiesto :: Int -> Bool
bisiesto a = a `mod` 4 == 0 && (a `mod` 100 /= 0 || (a `mod` 100 == 0 && a `mod` 400 == 0))






























