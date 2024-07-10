type Habilidades = (Float, Float)
data Participante = Participante {nombre :: String, curso :: String, habilidad :: Habilidades} deriving (Show)
type Obstaculo = (Tiro -> Bool, Tiro -> Tiro)

data Tiro = Tiro {velocidad :: Float, precision :: Float, altura :: Float} deriving (Show)

putter :: Habilidades -> Tiro
putter (fuerza, prec) = (Tiro (10) (prec * 2) (0))

madera :: Habilidades -> Tiro
madera (fuerza, prec) = (Tiro 100 (prec / 2) 5)

hierro :: Int -> Habilidades -> Tiro
hierro n (fuerza, prec) = (Tiro (fuerza * fromIntegral (n)) (prec / fromIntegral(n)) (fromIntegral (n * n)))

golpe :: Participante -> (Habilidades -> Tiro) -> Tiro
golpe alumno palo = palo (habilidad alumno)

palos :: [Habilidades -> Tiro]
palos = (putter : madera : map hierro [1..10])

puedeSuperarConAlgunPalo :: Participante -> Obstaculo -> Bool
puedeSuperarConAlgunPalo alumno (funcion, nuevoTiro) = puedeSuperarAux alumno palos funcion

puedeSuperarAux :: Participante -> [(Habilidades -> Tiro)] -> (Tiro -> Bool) -> Bool
puedeSuperarAux _ [] _ = False
puedeSuperarAux alumno (h:t) func | func (golpe alumno h) == True = True
                                  | otherwise = puedeSuperarAux alumno t func

nombresDeLosQueSuperanTodo :: [Participante] -> [Obstaculo] -> [String]
nombresDeLosQueSuperanTodo [] _ = []
nombresDeLosQueSuperanTodo (h:t) obs | puedeSuperar h obs > 0 = [nombre h] ++ nombresDeLosQueSuperanTodo t obs
                                     | otherwise = nombresDeLosQueSuperanTodo t obs

puedeSuperar :: Participante -> [Obstaculo] -> Int
puedeSuperar _ [] = 0
puedeSuperar alumno (h:t) | puedeSuperarConAlgunPalo alumno h == True = 1 + puedeSuperar alumno t
                          | otherwise = puedeSuperar alumno t

alumnoGanador :: [Participante] -> [[Obstaculo]] -> String
alumnoGanador (h:t) obst = mayor (h:t) obst h 0 

mayor :: [Participante] -> [[Obstaculo]] -> Participante -> Int -> String
mayor [] obst alumno puntaje = nombre alumno
mayor (h:t) obst alumno puntaje | compite h obst > puntaje = mayor t obst h (compite h obst)
                           | otherwise = mayor t obst alumno puntaje

compite :: Participante -> [[Obstaculo]] -> Int
compite _ [] = 0
compite alumno (h:t) = puedeSuperar alumno h + compite alumno t 
