extraerEntre :: Int -> Int -> [a] -> [a]
extraerEntre _ _ [] = []
extraerEntre i k ls
    | i >= length ls = []
    | i < k = ls !! i : extraerEntre (i+1) k ls
    | otherwise = []

luhn :: [Int] -> Bool
luhn l = last(separarDigitos(sumaDigitos(modificarPosicionesImpares(reverse l)))) == 0

modificarPosicionesImpares :: [Int] -> [Int]
modificarPosicionesImpares [] = []
modificarPosicionesImpares l = modificarPosicionesImparesAux 0 l

modificarPosicionesImparesAux :: Int -> [Int] -> [Int]
modificarPosicionesImparesAux _ [] = []
modificarPosicionesImparesAux i (l:ls)
       | odd i = (l*2) : modificarPosicionesImparesAux (i+1) ls
       | otherwise = l : modificarPosicionesImparesAux (i+1) ls
       
sumaDigitos :: [Int] -> Int
sumaDigitos ls = sum [ sum(separarDigitos l) | l <- ls] 

separarDigitos :: Int -> [Int]
separarDigitos n = map digitToInt (show n)
  where
    digitToInt :: Char -> Int
    digitToInt c = read [c]

ultimoDigito :: [a] -> [a]
ultimoDigito l = extraerEntre ((length l)-1) (length l) l

separarDigitos' :: Int -> [Int]
separarDigitos' n = [ read [x] | x <- show(n)] 
    


