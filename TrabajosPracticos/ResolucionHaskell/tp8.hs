
--Ejercicio 1
--A

--Version 1
volEsfera r = (4/3)* pi * r^3
--Version 2
volumen :: Float -> Float
volumen r = pi * (4/3) * r^3

--B
--Version 1
sumaCoins a b c d e = (a*0.01)+(b*0.05)+(c*0.1)+(d*0.5)+e

--Version 2
sumaCoins2 :: Int -> Int -> Int -> Int -> Int -> Float
sumaCoins2 a b c d e = fromIntegral(a + b*5 + c*10 + d*50 + e*100)/100

--C
--Version 1
incrementarTupla (a,b,c) = (a+1,b+1,c+1)

--Version 2
incrementarTupla2 :: Num a => [a] -> [a]
incrementarTupla2 xs = [x + 1 | x <- xs]

--D
--Version 1
numCuadrado x = x*x

--Version 2
cuadrado :: Num a => a -> a
cuadrado x = x^2


--E
--Version 1
potNum4 x = (numCuadrado x) * (numCuadrado x)

--Version 2
elevadoALaCuarta :: Num a => a -> a
elevadoALaCuarta x = cuadrado(cuadrado(x))

--F
Version 1
mediaAritmetica x1 x2 x3 = (x1+x2+x3)/3

--Version 2
mediaAritmetica3 :: Floating a => (a,a,a) -> a
mediaAritmetica3 (a,b,c) = (a + b + c)/3 

--G
--Version 1
max3 x1 x2 x3 = max (max x1 x2) x3

--Version 2
maximo3 :: (Floating a, Ord a) => a -> a -> a -> a
maximo3 a b c = max a (max b c)

--H
--Version 1
max6 x1 x2 x3 x4 x5 x6 = max (max3 x1 x2 x3) (max3 x4 x5 x6) 

--Version 2
maximo6 :: (Floating a, Ord a) => a -> (a -> (a -> (a -> (a -> (a -> a)))))
maximo6 a b c d e f = max (maximo3 a b c) (maximo3 d e f)

--I
areaTriangulo a b c = sqrt (s* (s-a) * (s-b) * (s-c)) --Fórmula de Heron
             where
                  s = (a+b+c)/2 -- Semiperimetro del triangulo

--J
--Version 1
cuadrante (x,y) | (x>0) && (y>0) = 1
                | (x<0) && (y>0) = 2
                | (x<0) && (y<0) = 3
                | (x>0) && (y<0) = 4

--Version 2
cuadrante :: (Floating a, Ord a) => (a,a) -> a
cuadrante (a, b) 
         |a>0 && b>0 = 1
		 |a<0 && b>0 = 2
		 |a<0 && b<0 = 3
		 |a>0 && b<0 = 4
		 |otherwise = 0

--K
igualesTres :: Eq a => a -> (a -> (a -> Bool))
igualesTres x y z = (x == y) && (y == z)

--L
--Version 1
diferentesTres x y z | (x /= y) && (y /= z) && (x /= z) = True
                     | otherwise = False

--Version 2
diferentesTres :: Eq a => a -> (a -> (a -> Bool))
diferentesTres a b c = a /= b && b /= c && a /= c

--M
--Version 1
igualesCuatro v x y z = (numCuadrado v == numCuadrado x) && (numCuadrado x == numCuadrado y) && (numCuadrado y == numCuadrado z)

--Version 2
igualesCuatro :: Eq a => a -> (a -> (a -> (a -> Bool)))
igualesCuatro a b c d = igualesTres a b c && c == d

--N
--Version 1
raices :: (Float , Float , Float) -> (Float , Float)
raices (a,b,c) = (r1 , r2) where
            r1 = e + sqrt z /(2*a)
            r2 = e - sqrt z /(2*a)
            z = numCuadrado b - (4*a*c)
            e = -b/(2*a)

--Version 2
{-
raices :: (Floating a, Ord a) => a -> a -> a -> [a]
raices a b c
  | discriminante < 0 = []
  | otherwise = [(-b + sqrt discriminante) / (2*a), (-b - sqrt discriminante) / (2*a)]
  where discriminante = b^2 - 4*a*c
-}

--Ñ
bisiesto :: Int -> Bool
bisiesto x = (x `mod` 4 == 0) && (x `mod` 100 /= 100 || x `mod` 400 == 0)

--O
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False


-- Ejercicio 2
--A
divisores :: Integer -> [Integer]
divisores n = [i | i <- [1..n] , n `mod` i == 0] ++ [i | i <- [1..abs n] , n `mod` i == 0]

--B
timer :: Integer -> (Integer , Integer , Integer)
timer total = (horas , minutos , segundos)
          where horas = div total 3600
                minutos = div (mod total 3600) 60
                segundos = mod total 60

primo :: Integer -> Bool
primo n = (length (divisores n) == 4) --Tiene que ser divisible por 1 o por si mismo, pero divisores tambien los devuelve negativo

listaPrimos :: Integer -> [Integer]
listaPrimos n = [i | i <- [1..n] , primo i]

tomar :: Integer -> [a] -> [a]
tomar 0 _ = []
tomar _ [] = []
tomar n (x:xs) = x : tomar (n-1) xs

tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] = []
tomarMientras p (x:xs)
                   | p x = x : tomarMientras p xs
                   | otherwise = tomarMientras p xs

nIndex :: [a] -> Int -> a
nIndex [] _ = error "Indice fuera de rango"  
nIndex (x:xs) n
    | n < 0     = error "El índice es negativo."  -- Si el índice es negativo, genera un error.
    | n == 0    = x  -- Cuando n es 0, hemos encontrado el elemento deseado.
    | otherwise = nIndex xs (n - 1)  -- Continúa buscando en el resto de la lista con un índice decrementado.
    
    
element :: Eq a => a -> [a] -> Bool
element _ [] = False
element e (x:xs)
     |(e == x) = True
     | otherwise = element e xs
     
binario :: Integer -> [Integer]
binario 0 = [0]
binario n
                | n>0 = reverse(binario_aux n)
                | otherwise = 1: reverse(binario_aux (-n))

binario_aux :: Integer -> [Integer]
binario_aux 0 = [0]
binario_aux n = (n `mod` 2) : binario_aux (n `div` 2)


-- Definición de un nuevo tipo de datos para números complejos
data Complejo = Complejo { realPart :: Double, imagPart :: Double }
    deriving (Show)

-- Función para sumar dos números complejos
sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos c1 c2 = Complejo (realPart c1 + realPart c2) (imagPart c1 + imagPart c2)

-- Función para multiplicar dos números complejos
multiplicaComplejos :: Complejo -> Complejo -> Complejo
multiplicaComplejos c1 c2 = Complejo
    { realPart = (realPart c1 * realPart c2) - (imagPart c1 * imagPart c2)
    , imagPart = (realPart c1 * imagPart c2) + (imagPart c1 * realPart c2)
    }

data Color = Rojo | Azul | Verde | Amarillo | Violeta | Celeste | Negro | Marron | Gris | Blanco  deriving (Eq , Show)

coloresValidos :: [Color]
coloresValidos = [Rojo , Azul , Verde , Amarillo , Violeta , Celeste , Negro , Marron , Gris , Blanco]

listaColores :: [Color] -> Bool
listaColores = all (\c -> c `elem` coloresValidos) 


ocurrencias :: Eq a => a -> [a] -> Int
ocurrencias elemento lista = length [x | x <- lista, x == elemento]

--Forma Recursiva
sumaPar :: [(Int , Int)] -> [Int]
sumaPar [] = [] 
sumaPar ((x , y):ls) = x+y: sumaPar ls

-- List Compresion
sumaPar2 :: [(Int , Int)] -> [Int]
sumaPar2 w = [x+y| (x,y) <- w]


zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []  xs = xs
zipMaximos xs [] = xs
zipMaximos (x1:xs1) (x2:xs2) = max x1 x2 : zipMaximos xs1 xs2

zipSort :: [Int] -> [Int] -> [(Int,Int)]
zipSort [] [] = []
zipSort (x1:xs1) (x2:xs2)
                      | (length (x1:xs1) == length (x2:xs2)) =  (min x1 x2, max x1 x2) : zipSort xs1 xs2
                      | otherwise = error "No se puede hacer, tienen longitud distintas"

data Pizza = Pizza {ingredientes :: String, precio :: Float} deriving(Show, Eq)
pizzas = [Pizza "Queso" 150 , Pizza "Tomate" 200 , Pizza "pepperoni" 202 , Pizza "Napolitana" 300]

dropPrecio :: [Pizza] -> [Pizza]
dropPrecio p = filter (\x -> (precio x) > 200) p


suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs

alguno :: [Bool]->Bool
alguno [] = False
alguno (x:xs)  
               |(x==True) = True
               |otherwise = alguno xs

todos :: [Bool]->Bool
todos []=True
todos (x:xs) |(x==False) = False
       |otherwise = todos xs
    
codigos :: [Char] -> [Int]
codigos [] = []
codigos (x:xs) = fromEnum x : codigos xs

restost :: [Int] -> Int -> [Int]
restost [] n = []
restost (x:xs) n = (x `mod` n) : restost xs n

incrementar :: [Int] -> [Int]
incrementar [] = []
incrementar (x:xs) = (x+1) : incrementar xs

cuadrados :: [Int] -> [Int]
cuadrados [] = []
cuadrados (x:xs) = (x*x) : cuadrados xs

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = (length x) : longitudes xs

orden :: [(Int,Int)] -> [(Int,Int)]
orden [] = []
orden ((x,y):ls)
    |(x<3*y) = (x,y) : orden ls
    |otherwise = orden ls
    
pares :: [Int] -> [Int]
pares [] = []
pares (x:xs)
      |x `mod` 2 == 0 = x : pares xs
      | otherwise = pares xs

letras :: [Char] -> [Char]
letras [] = []
letras (x:xs)
     |64 < (fromEnum x) && 91 > (fromEnum x) = x:letras xs
     |96 < (fromEnum x) && 123 > (fromEnum x) = x:letras xs
     |otherwise = letras xs
     
masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (x:xs) n
                        | length x >n = x : (masDe xs n)
                        | otherwise = masDe xs n

funciones :: [Int->Int] -> Int-> [Int]
funciones [] _ = []
funciones (f:f2) n = f n :  funciones f2 n

strToInt :: Int -> [Int]
strToInt n = map (\c -> read c :: Int) (show n)

findList :: Int -> [Int] -> Int
findList n [] = error "No se encontro el elemento"
findList n (l:ls) | n/=l = 1  + findList n ls
       |otherwise = 0

sumaCuadrados :: [Int] -> Int
sumaCuadrados xs = sum [x^2| x <- xs]


