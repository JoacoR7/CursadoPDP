data Guantelete = Guantelete {material :: String, gemas :: Int} deriving(Show)
data Personaje = Personaje {nombre :: String, edad :: Int, planeta :: String, energia :: Float, habilidades :: [String]} deriving(Show)
data Universo = Universo {planetas :: [String], habitantes :: [Personaje]} deriving(Show)

g1 = Guantelete "oro" 6
g2 = Guantelete "hierro" 6
p1 = Personaje "IronMan" 40 "Tierra" 100 ["Tener plata", "volar"]
p2 = Personaje "CapitanAmerica" 120 "Tierra" 200 ["Escudo"]
p3 = Personaje "Groot" 5 "Groot" 300 ["Groot"]
p4 = Personaje "Thor" 2000 "Midgard" 1000 ["Martillazo", "Rayo"]
p5 = Personaje "Hulk" 2500 "Tierra" 2000 ["Aplastar"]
u1 = Universo ["Tierra", "Groot", "Midgard"] [p1,p2,p3,p4,p5]

chasquido :: Guantelete -> Universo -> Universo
chasquido (Guantelete m g) (Universo p h)
       | m == "oro" && g == 6 = Universo p (take (div (length h) 2) h)
       | otherwise = Universo p h


mantener :: (a -> Bool) -> [a] -> [a]
mantener _ [] = []
mantener f (l:ls)
       | f l = l:(mantener f ls)
       | otherwise = mantener f ls

descartar :: (a -> Bool) -> [a] -> [a]
descartar _ [] = []
descartar f (l:ls)
       | f l = descartar f ls
       | otherwise = l:(descartar f ls)
