data Luchador = Luchador {nombre :: String, poderInicial :: Float, ki :: Float, tecnicas :: [Tecnica], entrenadores :: [String]} deriving(Show)

data Tecnica = Tecnica {nombreTec :: String, poder :: Float, kiNecesario :: Float} deriving(Show, Eq)

t1 = Tecnica "t1" 10 2
t2 = Tecnica "t2" 100 60
kameHame = Tecnica "KameHame" 200 10
morir = Tecnica "Morir" 0 10

l1 = Luchador "l1" 10 50 [t1, t2] ["mrSatan", "karin", "roshi"]

entrenadoresL (Luchador _ _ _ _ x) = x

poderTecnica :: Tecnica -> Float
poderTecnica (Tecnica _ p ki) = p - ki

poderLuchador :: Luchador -> Float
poderLuchador (Luchador _ pi ki ts _) = pi + ki + sum [x| t <- ts, let x = poderTecnica t]

entrenarLuchador :: Luchador -> Luchador
entrenarLuchador (Luchador n pi ki ts (e:es))
       | e == "roshi" && not (elem kameHame ts) = Luchador n pi ki (ts ++ [kameHame]) (e:es)
       | e == "karin" = Luchador n (pi + pi*0.2) (ki + ki*0.1) ts (e:es)
       | e == "mrSatan" = Luchador n pi ki [morir] (e:es)
       | otherwise = (Luchador n pi ki ts (e:es))
       
mejorEntrenador :: (String, String) -> Luchador -> String
mejorEntrenador (e1, e2) (Luchador n pi ki ts _)
    | poderLuchador (entrenarLuchador (Luchador n pi ki ts [e1])) >= poderLuchador (entrenarLuchador (Luchador n pi ki ts [e2])) = e1
    | otherwise = e2

entrenadoresOrdenados :: Luchador -> Bool
entrenadoresOrdenados (Luchador n pi ki ts es) =  and [(mejorEntrenador (x,y) (Luchador n pi ki ts es)) == y | (x, y) <- zip es (tail es)]
    
    
    
