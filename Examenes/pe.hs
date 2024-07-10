type Pollo = (String, Int, Int)

edad :: Pollo -> Float
edad (_, d, _) = fromIntegral(d) / (pi * (365*365))

esAdulto :: Pollo -> Bool
esAdulto p = (edad p) > 5

esJoven :: Pollo -> Bool
esJoven p = (edad p) < 5

estaDesnutrido :: Pollo -> Bool
estaDesnutrido (n, d, p)
       | esJoven (n, d, p) && p < 50 = True
       | esAdulto (n, d, p) && p < 200 = True
       | even (length n) = True
       | otherwise = False
       
engordar :: Int -> Pollo -> Pollo
engordar a (n, d, g) = (n, d, g+a)

alimentar :: Int -> Pollo -> Pollo
alimentar a (n, d, g)
       | estaDesnutrido (n, d, g) = engordar a (n, d, g)
       | esAdulto (n, d, g) = engordar (div a 2) (n, d, g)
       | otherwise = (n, d, g)
