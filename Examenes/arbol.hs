data Arbol = Arbol {especie :: String, altura :: Float, ancho :: Float, vitalidad :: Float} deriving(Show)

baobab = Arbol "baobab" 5 10 0
roble = Arbol "roble" 7 15 2
arbol = Arbol "arbol" 2 5 40

esFrondoso :: Arbol -> Bool
esFrondoso (Arbol _ al an vi) = al >= 6 && al <= 15 && al < an && vi > 1

esperanzaVida :: Arbol -> Float
esperanzaVida (Arbol _ _ _ vi) = vi * (45/2)

lluvia :: Float -> Arbol -> Arbol
lluvia n (Arbol e al an vi) = Arbol e (al+1) an (vi + vi * n/100)

granizo :: Arbol -> Arbol
granizo (Arbol e al an vi) = Arbol e (disminuirAltura al) an vi

disminuirAltura :: Float -> Float
disminuirAltura n
       | n <= 3 = 1
       | otherwise = n - 2
       
tormenta :: Arbol -> Arbol
tormenta a = granizo (lluvia 100 a)

fueBuenDia :: Arbol -> Bool
fueBuenDia a = (esperanzaVida(lluvia 150 a)) > 5
