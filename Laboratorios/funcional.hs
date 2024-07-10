--E3
data Personaje = Personaje {nombre :: String, edad :: Int, energia :: Float, planeta :: String, habilidades :: [String]} deriving (Show)

data Guantelete = Guantelete {material :: String, gemas :: Int} deriving (Show)

data Universo = Universo {habitantes :: [Personaje]} deriving (Show)

chasquido :: Guantelete -> Universo -> Universo
chasquido g u
 | material g == "oro" && gemas g == 6 = reducir u
 | otherwise = u

reducir :: Universo -> Universo
reducir u = Universo (take (div (length (habitantes u)) 2) (habitantes u))


ironMan = Personaje "ironMan" 42 100 "Tierra" ["volar", "fabricar"]
drStrange = Personaje "drStrange" 42 120 "Tierra" ["volar", "magia"]
groot = Personaje "groot" 4 40 "WhoKnows" ["yo", "soy", "groot"]
wolverine = Personaje "wolverine" 50 70 "Tierra" ["garras", "matar"]
viudaNegra = Personaje "viudaNegra" 35 110 "Tierra" ["volar", "magia"]

univ = Universo [ironMan, drStrange, groot, wolverine, viudaNegra]
guante = Guantelete "oro" 6



--E4
mantener :: [a] -> (a -> Bool) -> [a]
mantener [] p = []
mantener (x:xs) p
 | p x = x : mantener xs p
 | otherwise = mantener xs p

descartar :: [a] -> (a -> Bool) -> [a] 
descartar [] p = []
descartar (x:xs) p
 | not (p x) = x : descartar xs p
 | otherwise = descartar xs p



--E5
funcionHeavy a b c d e | d > e = map a c
                       | otherwise = map b c
