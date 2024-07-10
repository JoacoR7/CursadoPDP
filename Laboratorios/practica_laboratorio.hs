import Data.Bits
import Data.Char


esPangrama :: String -> Bool
esPangrama s = and (map (\x -> elem x (map toLower s)) ['a'..'z'])

data Alergia = Perro | Gato | Penicilina | Chocolate | Soja | Tomate | Polen | Pescado deriving (Enum, Show, Eq)

alergias :: Int -> [Alergia]
alergias n = filter (`esAlergicoA` n) [Perro .. Pescado]

esAlergicoA :: Alergia -> Int -> Bool
esAlergicoA a n = testBit n (fromEnum a)

-- Pinky y Cerebro
-- type se usa para dar sinónimos en vez de data
data Animal = Animal { iq :: Float, especie :: String, capacidades :: [String]} deriving (Show, Eq)


--inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual
inteligenciaSuperior :: Float -> Animal -> Animal
inteligenciaSuperior n a = Animal ((iq a) + n) (especie a) (capacidades a)

--pinkificar: quitarle todas las habilidades que tenía
pinkificar :: Animal -> Animal
pinkificar a = Animal (iq a) (especie a) []

--superpoderes: le da habilidades nuevas
superpoderes :: Animal -> Animal
superpoderes a | especie a == "elefante" = Animal (iq a) (especie a) (capacidades a ++ ["no tenerle miedo a los ratones"])
 | especie a == "raton" && iq a > 100 = Animal (iq a) (especie a) (capacidades a ++ ["hablar"])
 | otherwise = a


antropomorfico :: Animal -> Bool
antropomorfico a = iq a > 60 && elem "hablar" (capacidades a)

noTanCuerdo :: Animal -> Bool
noTanCuerdo a = tieneNCapacidadesPinkiescas (capacidades a) 2

tieneNCapacidadesPinkiescas :: [String] -> Int -> Bool
tieneNCapacidadesPinkiescas c n = length (filter pinkiesco c) >= n

pinkiesco :: String -> Bool
pinkiesco s = s == "sonido"


type Experimento = Experiment
data Experiment = Experiment {transf :: [(Animal -> Animal)], exito :: (Animal -> Bool)}

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso e a = exito e (aplicarTransf (transf e) a)

aplicarTransf :: [(Animal -> Animal)] -> Animal -> Animal
aplicarTransf (h:t) a | null t = h a
 | otherwise = aplicarTransf t (h a)


raton = Animal 17 "raton" ["destruenglonir el mundo", "hacer planes desalmados"]
ex = Experiment [pinkificar, inteligenciaSuperior 100, superpoderes] antropomorfico

