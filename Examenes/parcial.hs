data Personaje = Personaje {nombre:: String, edad :: Int, energia :: Int, habilidades :: [String], planeta :: String} deriving(Show, Eq)
data Guantelete = Guantelete {material :: String, gemas :: Int} deriving(Show,Eq)
data Universo = Universo {habitantes :: [Personaje]} deriving(Show,Eq)

chasquido :: Guantelete -> Universo -> Universo
chasquido g u
     |material g == "oro" && gemas g == 6 = Universo (take (div (length(habitantes u)) 2) (habitantes u))
     |otherwise = u

uni = Universo [Personaje "IronMan" 2 2 ["Volar"] "Tierra", Personaje "Spiderman" 10 3 ["Arañar"] "Tierra", Personaje "Bender" 123 123 ["Tomar"] "Tierra"]
guante = Guantelete "oro" 6

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
