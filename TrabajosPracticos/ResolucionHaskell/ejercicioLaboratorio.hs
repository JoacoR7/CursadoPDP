-- Ejercicio desaparecer Habitantes de un Universo
data Personaje = Personaje {nombre:: String, edad :: Int, energia :: Int, habilidades :: [String], planeta :: String} deriving(Show, Eq)
data Guantelete = Guantelete {material :: String, gemas :: Int} deriving(Show,Eq)
data Universo = Universo {habitantes :: [Personaje]} deriving(Show,Eq)

listaHabitantes = [Personaje "ironMan" 40 100 ["NOSE"] "Tierra" , Personaje "drStrange" 50 150 ["Nose"] "Tierra" , Personaje "groot" 55 150 ["Nose"] "Tierra"]


chasquido :: Guantelete -> Universo -> Universo
chasquido g u
     |material g == "oro" && gemas g == 6 = Universo (take (div (length(habitantes u)) 2) (habitantes u))
     |otherwise = u
     
mantener :: (a->Bool) -> [a] -> [a]
mantener f [] = []
mantener f (l:ls)
    |f l = l: mantener f ls
    |otherwise = mantener f ls

descartar :: (a->Bool) -> [a] -> [a]
descartar f [] = []
descartar f (l:ls)
    |not(f l) = l: descartar f ls
    |otherwise = descartar f ls
    
extraer :: Int -> Int -> [a] -> [a]
extraer i k l = [ l !! x| x <- [i..(k-1)]]

-- Ejercicio verificar numero de Luhn
digitosInv :: Integer -> [Integer]
digitosInv n = reverse (map (\c -> read [c] :: Integer) (show n))

doblePosImpar :: [Integer] -> [Integer]
doblePosImpar xs = [ aux (xs!!n) n | n <- [0 .. length xs -1] ]

aux :: Integer -> Int -> Integer
aux x n
 | (n `mod` 2 == 0) = x
 | otherwise = x*2

sumaDigitos :: [Integer] -> Integer
sumaDigitos xs = sum [sum (digitosInv n) | n <- xs]

ultimoDigito :: Integer -> Integer
ultimoDigito n = head (digitosInv n)

luhn :: Integer -> Bool
luhn n = ultimoDigito (sumaDigitos ( doblePosImpar (digitosInv n) )) == 0

listaNcopias :: [Int] -> [Int]
listaNcopias xs = [x | num <- xs, x <- generarNumeros num num]

generarNumeros :: Int -> Int -> [Int]
generarNumeros num 0 = []
generarNumeros num count = num : generarNumeros num (count - 1)

-- Ejercicio presentado en clase
data Persona = Persona {peso :: Float , colesterol :: Float} deriving(Show,Eq)
data Comida = Ensalada {kilos :: Float} | Hamburguesa {ingredientes :: [String]} | Palta

esEnsalada (Ensalada _) = True
esEnsalada _ = False

esHamburguesa (Hamburguesa _) = True
esHamburguesa _ = False

esPalta Palta = True
esPalta _ = False

comer :: Persona -> Comida -> Persona
comer p1 comida | esEnsalada comida = Persona (((kilos comida)/2) + peso p1) (colesterol p1)
                | esHamburguesa comida = Persona ((peso p1) + fromIntegral (length(ingredientes comida)) *3) ((colesterol p1) * 1.5)
                | esPalta comida = Persona ((peso p1)+2) (colesterol p1)
                
-- fromIntegral sirve para pasar de Int a Float

almorzar :: [Comida] -> Persona -> Persona
almorzar xs p1 = foldl comer p1 xs

--*Main> :t foldr
--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
--*Main> :t foldl
--foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b







