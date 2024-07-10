--Definir el tipo de dato animal
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
data Animal = Animal {iq :: Int, especie :: String, capacidades :: [String]} deriving(Show,Ord,Eq)

delfin = Animal 50 "delfin" ["nadar"]
caniche = Animal 25 "perro" ["ladrar"]

--2. Definir funciones
inteligenciaSuperior :: Animal -> Int -> Animal
inteligenciaSuperior (Animal iq e c) n = Animal (iq+n) e c

pinkificar :: Animal -> Animal
pinkificar (Animal iq e c) = Animal iq e []

superpoderes :: Animal -> Animal
superpoderes (Animal iq e c)
    |e == "elefante" = Animal iq e ("no tenerle miedo a los ratones" : c)
    |e == "raton" && iq > 100 = Animal iq e ("hablar" : c)
    |otherwise = Animal iq e c

--3. Mas funciones
antropomorfico :: Animal -> Bool
antropomorfico (Animal iq _ c)
    |iq > 60 && elem "hablar" c = True
    |otherwise = False

pinkiesco :: String -> Int
pinkiesco x
    |x == "feliz" || x == "adorable" = 1
    |otherwise = 0

noTanCuerdo :: Animal -> Bool
noTanCuerdo (Animal _ _ c)
    |sum (map pinkiesco c) > 1 = True
    |otherwise = False

--4. experimento

type Transformaciones = [Animal -> Animal]
type Criterio = (Animal -> Bool)

data Experimento = Experimento Transformaciones Criterio

sujeto :: Animal
sujeto = Animal 95 "raton" ["destruir el mundo", "hacer planes desalmados"]

exper :: Experimento
exper = Experimento [pinkificar, flip inteligenciaSuperior 10, superpoderes]  antropomorfico

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso (Experimento (tran:trans) criterio) a = experimentoExitoso (Experimento trans criterio) (tran a)
experimentoExitoso (Experimento [] criterio) a = criterio a