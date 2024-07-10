data Participante = Participante {nombre :: String, curso :: String, habilidades :: (Float, Float)} deriving(Show)

data Tiro = Tiro {velocidad :: Float, precision :: Float, altura :: Float} deriving (Show)

type Obstaculo = (Tiro -> Bool, Tiro -> Tiro)

data Palo = Putter | Madera | Hierro Int deriving (Show)

between :: Float -> Float -> Float -> Bool
between m n p = n >= m && n <= p

putter :: Participante -> Tiro
putter (Participante _ _ (f, p)) = Tiro 10 (p*2) 0

madera :: Participante -> Tiro
madera (Participante _ _ (f, p)) = Tiro 100 (p/2) 5

hierro :: Int -> Participante -> Tiro
hierro n (Participante _ _ (f,p))
       | n > 0 && n < 11 = Tiro (f * fromIntegral(n)) (p / fromIntegral(n)) (fromIntegral(n)^2)
       | otherwise = error("Número incorrecto de hierro")

golpe :: Participante -> (Participante -> Tiro) -> Tiro
golpe p palo = palo p

hoyo :: Obstaculo
hoyo = ((\(Tiro v p a) -> between 5 20 v && p > 95 && a == 0), (\ _ -> Tiro 0 0 0))

palos :: [Participante -> Tiro]
palos = [putter, madera] ++ map hierro [1..10]

puedeSuperarConAlgunPalo :: Participante -> Obstaculo -> Bool
puedeSuperarConAlgunPalo p (condicion, tiro) = length [x | palo <- palos, let x = condicion(golpe p palo), x] > 0

nombresDeLosQueSuperanTodo :: [Participante] -> [Obstaculo] -> [String]
nombresDeLosQueSuperanTodo participantes obstaculos = [show(x) | x <- participantes, superanAux x obstaculos]

superanAux :: Participante -> [Obstaculo] -> Bool
superanAux _ [] = True
superanAux p (o:os) = (puedeSuperarConAlgunPalo p o) && superanAux p os

alumnoGanador :: [Participante] -> [[Obstaculo]] -> String
alumnoGanador p o
       | res == [] = []
       | otherwise = head res
       where
       res = foldl1 interseccion [nombresDeLosQueSuperanTodo p x | x <- o]

interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion (x:xs) ys
  | x `elem` ys = x : interseccion xs ys
  | otherwise   = interseccion xs ys

