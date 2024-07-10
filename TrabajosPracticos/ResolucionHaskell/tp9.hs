import Data.Char (toUpper, toLower)
sumaCuadrados :: [Int] -> Int
sumaCuadrados xs = sum [x^2| x <- xs]

sumaCuadradosImpar :: [Int] -> Int
sumaCuadradosImpar xs = sum [x^2| x <- xs , not (even x)]

sumaCuadradosPar :: [Int] -> Int
sumaCuadradosPar xs = sum [x^2| x <- xs , even x]

numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [0..n] , even x]

numerosPares2 :: Int -> Int -> [Int]
numerosPares2 n m = [x | x <- [0..n] , even x && x > m]

divisores :: Int -> [Int]
divisores n = [x| x <- [1..n], n `mod` x == 0]

listaNcopias :: [Int] -> [Int]
listaNcopias xs = [x |x <- (generarNumeros num num) num <- xs]

generarNumeros :: Int -> Int-> [Int]
generarNumeros num 0 = []
generarNumeros num count = num : generarNumeros num (count-1)

cantidadPares :: (Int, Int, Int) -> Int
cantidadPares (x,y,z) = length [n | n <- [x,y,z], even n]

primos:: Int->[Int]
primos n = [x |x <- [1..n] , length (divisores x)==2]

tuplas :: Int -> [(Int , Int , Int)]
tuplas n = [(x,y,z)|x <- [0..n] , y <- [0..n] , z <- [0..n] , x^2 + y^2 == z^2]

--Numero perfectos
numerosP :: Int -> [Int]
numerosP n =[x|x<- [1..n], (sum (divisores (x))-x) == x]

--Producto escalar
productoE :: [Int] -> [Int] -> Int
productoE xs ys = sum [x * y| (x, y) <- zip xs ys  ]

buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama l pos lon ps = [p | p <- ps, length p == lon, head(drop pos p) == l]

-- Posiciones en donde aparece un cierto caracter en un String
posiciones :: String -> Char -> [Int]
posiciones s c = [n|n <- [0..length s -1], head(drop n s) == c]

-- Convierte una lista de Strings al formato Titulo
titulo :: [String] -> [String]
titulo (p:ps) = (toUpper (head p) : map toLower (tail p)) : [funcionAux x | x <- ps]

funcionAux :: String -> String
funcionAux p
    | length p >= 4 = toUpper (head p) : map toLower (tail p)
    | otherwise = map toLower p
    
-- Suma una lista de enteros
sum2 :: [Int]->Int
sum2 ls = foldr (+) 0 ls

-- Multiplica una lista de enteros
producto :: [Int]->Int
producto ls = foldr (*) 1 ls

elementosAlineados :: Eq a => [a] -> [a] -> Int
elementosAlineados xs ys = length (filter (\x -> x == True) (zipWith (==) xs ys))
   
aplicarFuncion :: [a] -> (a -> b) -> [b]
aplicarFuncion ls f = map f ls

menores5 :: [Int] -> [Int]
menores5 = filter (\x -> x < 5)

--Multiplica todos los elementos mayores a 0 por 2
x02 :: [Int] -> [Int]
x02 ls = map (*2) (filter (\x -> x > 0 ) ls) 

-- Aplica la funcion 2 veces sobre el argumento
dosVeces :: (a -> a) -> a -> a
dosVeces f x = f (f x)

--2.2
data Planeta = Planeta {nombre :: String , aniosTerrestres :: Float} deriving(Eq,Show) 

planetas = [Planeta "Mercurio" 0.2408467, Planeta "Venus" 0.61519726, Planeta "Marte"  1.8808158]

edad :: String -> Float -> Float
edad p s = (s/31557600)/(aniosTerrestres (head (filter (\x -> nombre x == p) planetas)))

--2.4
--isbn :: String -> Bool
isbn s
       | longitud == 9 && last s == 'X' = True
       | longitud == 9 && (((fromEnum (last s)) > 47) && ((fromEnum (last s)) < 58)) = True
       | otherwise = False
       where longitud = length(filter filtrarDigitos (tail (reverse s)))
       
--take(((length s) -1) s)

--calcularIsbn :: String -> Int


--filtrarGuiones :: String -> String
--filtrarGuiones s = filter (\x -> x /= '-') s

--filtrarDigitos :: String -> String
filtrarDigitos s = filter (\x -> ((fromEnum x) > 47) && ((fromEnum x) < 58)) s
