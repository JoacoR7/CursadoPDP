quedanBotellas :: (Ord a, Num a, Show a) => a -> String -> IO ()
quedanBotellas cantidad bebida
  | cantidad > 1 = do
putStrLn $ show cantidad ++ " botellas de " ++ bebida ++ " en la pared, " ++ show cantidad ++ " botellas de " ++ bebida ++ ". Una se cayó y quedaron " ++ show (cantidad - 1) ++ " botellas de " ++ bebida ++ " en la pared."
botellasDeRecursiva bebida (cantidad - 1)
  | cantidad == 1 = do
putStrLn $ show cantidad ++ " botella de " ++ bebida ++ " en la pared, " ++ show cantidad ++ " botella de " ++ bebida ++ ". Una se cayó, no más botellas de " ++ bebida ++ " en la pared."
botellasDeRecursiva bebida (cantidad - 1)
  | otherwise = putStrLn $ "No hay más botellas de " ++ bebida ++ " en la pared, no más botellas de " ++ bebida

botellasDe :: String -> IO ()
botellasDe bebida = botellasDeRecursiva bebida 99

botellasDeRecursiva :: (Ord a, Num a, Show a) => String -> a -> IO ()
botellasDeRecursiva bebida cantidad
  | cantidad > 1 = do
quedanBotellas cantidad bebida
  | otherwise = putStrLn $ "Ve a la tienda y compra más, 99 botellas de " ++ bebida ++ " en la pared"
  
permutacionesRecursiva (x:xs) longitud
  |(longitud /= 0) = do (x:xs):permutacionesRecursiva (tail (x:xs):head(x:xs)) (longitud - 1)
  |otherwise = []
