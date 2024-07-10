type Material = String
data Personaje = Personaje {nombre :: String, puntaje :: Int, inventario :: [Material]} deriving(Show)
data Receta = Receta {receta :: String, materiales :: [Material], tiempo :: Int} deriving(Show)

p1 = Personaje "maikra" 1000 ["sueter", "fogata", "pollo", "pollo"]
r1 = Receta "pollo asado" ["fogata", "pollo"] 300

craftear :: Personaje -> Receta -> Personaje
craftear (Personaje n p i) (Receta r m t)
       | elementosNecesarios m i = Personaje n (p + 10*t) ((eliminarOcurrencias i m) ++ [r])
       | otherwise = Personaje n (p - 100) i
       
elementosNecesarios :: [Material] -> [Material] -> Bool
elementosNecesarios [] _ = True
elementosNecesarios (m:ms) ma = elem m ma && elementosNecesarios ms ma

eliminarOcurrencias :: Eq a => [a] -> [a] -> [a]
eliminarOcurrencias [] _ = []
eliminarOcurrencias lista [] = lista
eliminarOcurrencias lista (x:xs) = eliminarOcurrencias (eliminarOcurrencia lista x) xs

eliminarOcurrencia :: Eq a => [a] -> a -> [a]
eliminarOcurrencia [] _ = []
eliminarOcurrencia (y:ys) x
    | y == x = ys
    | otherwise = y : (eliminarOcurrencia ys x)

listaRecetas :: Personaje
