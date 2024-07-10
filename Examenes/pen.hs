type Pollo = (String, Int, Int, [String])
type Raton = (Int, Int, Int)
data Planeta = Planeta {entrenador :: (Pollo -> Pollo), pollos :: [Pollo]}
instance Show Planeta where
    show (Planeta e ps) = "Planeta {entrenador = <function>, pollos = " ++ show ps ++ "}"

p1 = Planeta (brujaTapita (10, 10, 10)) [("p1", 10, 10, ["saltar"])]

edad :: Pollo -> Float
edad (_, d, _, _) = fromIntegral(d) / (pi * (365*365))

esAdulto :: Pollo -> Bool
esAdulto p = (edad p) > 5

esJoven :: Pollo -> Bool
esJoven p = (edad p) < 5

estaDesnutrido :: Pollo -> Bool
estaDesnutrido (n, d, p, h)
       | esJoven (n, d, p, h) && p < 50 = True
       | esAdulto (n, d, p, h) && p < 200 = True
       | even (length n) = True
       | otherwise = False
       
engordar :: Int -> Pollo -> Pollo
engordar a (n, d, g, h) = (n, d, g+a, h)

alimentar :: Int -> Pollo -> Pollo
alimentar a (n, d, g, h)
       | estaDesnutrido (n, d, g, h) = engordar a (n, d, g, h)
       | esAdulto (n, d, g, h) = engordar (div a 2) (n, d, g, h)
       | otherwise = (n, d, g, h) 

arguiniano :: Pollo -> Pollo
arguiniano p = engordar 100 p

miyagi :: Pollo -> Pollo
miyagi (n, d, g, h)
       | not (elem "karate" h) = (n, d, g, h ++ ["karate"])
       | otherwise = (n, d, g, h)

marcelito :: Pollo -> Pollo
marcelito (n, d, g, _) = (n, d, g, [])

brujaTapita :: Raton -> Pollo -> Pollo
brujaTapita (p, a, b) (n, d, g, h) = engordar (p * a - b) (n, d, g, h)

marioBross :: String -> Pollo -> Pollo
marioBross habilidad (n, d, g, h)
       | not (elem habilidad h) && not (elem "saltar" h) = (n ++ "super mario", d, g, h ++ [habilidad, "saltar"])
       | not (elem habilidad h) && elem "saltar" h = (n ++ "super mario", d, g, h ++ [habilidad])
       | otherwise = (n ++ "super mario", d, g, h ++ ["saltar"])
       
marcenano :: Pollo -> Pollo
marcenano p = arguiniano (marcelito p)

esDebil :: Planeta -> Bool
esDebil (Planeta e []) = True
esDebil (Planeta e ((n, d, g, h):ps))
       | (length h) < 3 = True && esDebil (Planeta e ps)
       | otherwise = False

entrenar :: Planeta -> [Pollo]
entrenar (Planeta e ps) = [e x | x <- ps]

hacerViajeEspiritual :: Pollo -> [(Pollo -> Pollo)] -> Pollo
hacerViajeEspiritual p [] = p
hacerViajeEspiritual p (e:es) = hacerViajeEspiritual (e p) es


chickenNorris :: Pollo
chickenNorris = ("Chiken Norris", 9000000, 100000, listaInfinitaKarate)

listaInfinitaKarate :: [String]
listaInfinitaKarate = map (\n -> "karate" ++ show n) [1..10] --para infinito es [1..]

