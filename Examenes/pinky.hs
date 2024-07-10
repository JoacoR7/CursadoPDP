{-
data Animal = Animal {coeficiente :: Float , especie :: String , capacidades :: [String]} deriving(Show,Eq)

inteligenciaSuperior :: Animal -> Float -> Animal
inteligenciaSuperior animal n = animal {coeficiente = (coeficiente animal) + n}

pinkificar :: Animal -> Animal
pinkificar (Animal c e cap) = Animal c e []

superpoderes :: Animal -> [String] -> Animal
superpoderes (Animal iq e c) habilidades
   | e == "elefante" = Animal iq e (c++habilidades++["no tener miedo a los ratones"])
   | (e == "raton") && (iq > 100) = Animal iq e (c++habilidades++["Hablar"])
   | otherwise = Animal iq e (c++habilidades)
-}

data Animal = Animal {especie :: String, ci :: Float, capacidades :: [String]} deriving(Show)
data Experimento = Experimento {transformaciones :: [Transformacion], criterio :: String}
instance Show Experimento where
    show (Experimento ts c) = "Experimento {transformaciones = <function>, criterio = " ++ show c ++ "}"

type Transformacion = Animal -> Animal

a1 = Animal "Gato" 100 ["araniar", "tirar cosas de la mesa"]
a2 = Animal "elefante" 300 ["saltar"]
a3 = Animal "raton" 110 []
a4 = Animal "raton" 110 ["hablar"]

inteligenciaSuperior :: Animal -> Float -> Animal
inteligenciaSuperior (Animal e ci cap) n = Animal e (ci + n) cap

pinkificar :: Animal -> Animal
pinkificar (Animal e ci _) = Animal e ci []

superpoderes :: Animal -> Animal
superpoderes (Animal e ci cap)
       | e == "elefante" = Animal e ci (cap ++ ["no tenerle miedo a los ratones"])
       | e == "raton" && ci > 100 && notElem "hablar" cap = Animal e ci (cap ++ ["hablar"])
       | otherwise = (Animal e ci cap)

clasificarAnimal :: Animal -> String
clasificarAnimal (Animal e ci cap)
       | (elem "hablar" cap) && ci > 60 = "antropomorfico"
       | length (filter pinkiesco cap) > 2 = "no tan cuerdo"
       | otherwise = "animal normal"

pinkiesco :: String -> Bool
pinkiesco n = n == ""

experimentoExitoso :: Animal -> Experimento -> Bool
experimentoExitoso a (Experimento t c) = (clasificarAnimal a == c) 

experimentar :: Animal -> Experimento -> Animal
experimentar a _ = a
experimentar a (Experimento (t:ts) c) = experimentar (t a) (Experimento ts c)

