type Kilos = Int
type Ingrediente = String

data Persona = Persona {peso :: Kilos, colesterol :: Float} deriving(Show)
data Comida = Ensalada Kilos | Hamburguesa [Ingrediente] | Palta deriving(Show, Eq)

p1 = Persona 90 10
p2 = Persona 50 50
c1 = Ensalada 10
c2 = Hamburguesa ["Pan", "lechuga", "queso"]
c3 = Palta

comer :: Persona -> Comida -> Persona
comer (Persona p c) (Ensalada kilos) = Persona (p + div kilos 2) c
comer (Persona p c) (Hamburguesa ingredientes) = Persona (p + 3*(fromIntegral(length ingredientes))) (c*1.5)
comer (Persona p c) Palta = Persona (p+2) c

almorzar :: Persona -> [Comida] -> Persona
almorzar p [] = p
almorzar p (c:cs) = almorzar (comer p c) cs

almorzar' :: Persona -> [Comida] -> Persona
almorzar' p c = foldr (flip comer) p c

contieneComida :: [Comida] -> Comida -> Bool
contieneComida [] _ = False
contieneComida (l:ls) c
       | l == c = True
       | otherwise = contieneComida ls c
       
esSabrosa :: Comida -> Bool
esSabrosa (Ensalada kilos) = kilos > 1
esSabrosa (Hamburguesa ingredientes) = elem "cheddar" ingredientes
esSabrosa Palta = True

fitnjoy :: Persona -> Comida -> Bool
fitnjoy (Persona p c) comida = p < 80 && c < 100 && even p && esSabrosa comida
