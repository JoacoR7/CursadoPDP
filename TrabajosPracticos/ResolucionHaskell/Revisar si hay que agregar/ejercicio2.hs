--A


--G
nIndex :: [a] -> Int -> a
nIndex [] _ = error "La lista está vacía ó el índice fuera de rango"
nIndex (x:xs) n 
        | n<0 = error "EL índice es negativo."
        | n == 0 = x
        |otherwise = nIndex xs (n-1)

--H
eleme :: Eq a => a -> [a] -> Bool
eleme _ [] = False
eleme e (x:xs)
             | e == x = True
             |otherwise = eleme e xs

--I
binario :: Int -> [Int]
binario 0 = [0]
binario n
     |n>0 = reverse(binario_aux n)
     |otherwise = 1: reverse(binario_aux (-n))

binario_aux :: Int -> [Int]
binario_aux 0 = []
binario_aux n = (n `mod` 2): binario_aux (n `div` 2)

--J
data Complejo = Complejo {parteReal :: Double, parteImaginaria :: Double} deriving (Show)

sumaComplejos :: Complejo -> Complejo -> Complejo
sumaComplejos c1 c2 = Complejo (parteReal c1 + parteReal c2) (parteImaginaria c1 + parteImaginaria c2)

multiplicaComplejos :: Complejo -> Complejo -> Complejo
multiplicaComplejos c1 c2 = Complejo {parteReal = (parteReal c1 * parteReal c2) - (parteImaginaria c1 * parteImaginaria c2), parteImaginaria = (parteReal c1 * parteImaginaria c2) + (parteImaginaria c1 * parteReal c2)}

--K
{-
data Color = Rojo | Azul | Verde | Amarillo | Violeta | Celeste | Negro | Marron | Gris | Blanco deriving(Eq, Show)

coloresValidos :: [Color]
coloresValidos = [Rojo, Azul, Verde, Amarillo, Violeta, Celeste, Negro, Marron, Gris, Blanco]

verificarColores :: [Color] -> [Color] -> Bool
verificarColores lista = all (`elem` coloresValidos) lista
-}
--M
ocur :: Eq a => a -> [a] -> Int
ocur _ [] = 0
ocur e (x:xs)
       |e == x = 1 + ocur e xs
       |otherwise = ocur e xs

