--[] {}
--A
sumaPar :: [(Int, Int)] -> [Int]
sumaPar [] = []
sumaPar((x,y):ls) = (x+y): sumaPar ls

--B
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] xs = xs
zipMaximos xs [] = xs
zipMaximos (x1:xs1) (x2:xs2) = max x1 x2: zipMaximos xs1 xs2

--C 
zipSort :: [Int] -> [Int] -> [(Int, Int)]
zipSort [] [] = []
zipSort (x1:xs1) (x2:xs2)
       | (length xs1) == (length xs2) = (min x1 x2, max x1 x2): zipSort xs1 xs2
       | otherwise = error "Las listas son de distinta longitud"
       
--D


--E
data Pizza = Pizza{ingredientes :: String, precio :: Float} deriving(Show, Eq)

dropPrecio :: [Pizza] -> [Pizza]
dropPrecio p = filter (\x -> (precio x) > 200 ) p 
