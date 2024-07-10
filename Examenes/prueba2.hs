conjuncion x
      | True && x = x
      | False && x = False
      
longitud xd =  [1 | _ <- xd]

suma' x y = x + y

mult x y z = x*y*z

suc :: Int -> Int
suc = suma' 1

between menor mayor nro = menor <= nro && nro <= mayor

mult' :: Int -> Int -> Int
mult' x = \y -> x*y
