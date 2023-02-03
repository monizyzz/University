{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant if" #-}

-- 1. 
data Hora = H Int Int
          deriving Show
type Etapa = (Hora,Hora)
type Viagem = [Etapa]

-- (a)
etapaValida :: Etapa -> Bool 
etapaValida (h1,h2) = hDepois' h1 h2

hDepois' :: Hora -> Hora -> Bool
hDepois' (H x1 y1) (H x2 y2) | x1 > x2 = True
                             | x1 < x2 = False
                             | x1 == x2 && y1 > y2 = True
                             |otherwise = False 

-- (b)
viagemValida :: Viagem -> Bool
viagemValida [] = True 
viagemValida [x] = etapaValida x 
viagemValida (e1:e2:t) = etapaValida e1 && etapaValida (snd e1, fst e2) && viagemValida (e2:t)

--(c)
chegadapartida :: Viagem -> Etapa
chegadapartida [x] = x
chegadapartida x = (fst (head x), snd (last x))

-- (d)
tempoViagem :: Viagem -> Int 
tempoViagem [] = 0
tempoViagem ((h1,h2):t) = difM' h2 h1 + tempoViagem t

difM' :: Hora -> Hora -> Int
difM' (H x1 y1) (H x2 y2) = (abs(x1-x2)*60) + abs (y1-y2)

-- (e)
tempoEspera :: Viagem -> Int 
tempoEspera (e1:e2:t) = convH' (fst e2) - convH' (snd e1) + tempoEspera (e2:t)
tempoEspera [e] = 0 

convH' :: Hora -> Int
convH' (H x y) = x*60 + y

-- (f)
tempoTotal :: Viagem -> Int 
tempoTotal _ = 0 
tempoTotal l = tempoEspera l + tempoViagem l

-- 2.  
data Ponto = Cartesiano Double Double
             deriving (Show,Eq)            
type Poligonal = [Ponto]

-- (a)
dist' :: Ponto -> Ponto -> Double
dist' (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

linhaPoligonal :: Poligonal -> Double
linhaPoligonal (x:y:xs) = dist' x y + linhaPoligonal (y:xs) 
linhaPoligonal _ = 0

-- (b)
linhaFechada :: Poligonal -> Bool 
linhaFechada l = if (dist' (head l) (last l)) == 0 then True else False

-- (c)
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

triangula :: Poligonal -> [Figura]
triangula (x:y:z:xs) = (Triangulo x y z) : triangula (x:z:xs)
triangula _ = []

-- (d)
area :: Figura -> Double
area (Triangulo p1 p2 p3) = let a = dist' p1 p2
                                b = dist' p2 p3
                                c = dist' p3 p1
                                s = (a+b+c) / 2 -- semi-perimetro
                                in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Circulo a r) = pi * (r)^2
area (Rectangulo a b) = if posx a > posy b then (posx a - posx b) * (posy a - posy b) else (posx b - posx a) * (posy b - posy a)  


areaPoligonal :: Poligonal -> Double 
areaPoligonal l = aux (triangula l)
                  where aux :: [Figura] -> Double 
                        aux [] = 0
                        aux (x:xs) = area x + aux xs

-- (e)
mover :: Poligonal -> Ponto -> Poligonal
mover ((Cartesiano x1 y1):t) (Cartesiano x2 y2) = aux' ((Cartesiano x1 y1):t) (Cartesiano x2 y2) x2 y2
                                            where aux' :: Poligonal -> Ponto -> Double -> Double -> Poligonal
                                                  aux' a (Cartesiano x2 y2) x3 y3 = (Cartesiano x2 y2) : aux'' a x3 y3 
                                                  aux' [] _ _ _ = []
mover p _ = p

aux'' :: Poligonal -> Double -> Double -> Poligonal
aux'' ((Cartesiano x1 y1):t) x2 y2 = (Cartesiano (x1+x2) (y1+y2)) : aux'' t x2 y2 
aux'' [] _ _ = []

-- (f)
zoom :: Double -> Poligonal -> Poligonal
zoom  z [p1@(Cartesiano x y),p2@(Cartesiano a b)] = p1:(Cartesiano (z*a) (z*b)):[]
zoom z (p1@(Cartesiano x y):p2@(Cartesiano a b):pol) = p1:zoom z (p3:pol)
    where p3 = (Cartesiano (z*a) (z*b))

-- 3.  
data Contacto = Casa Integer
              | Trab Integer
              | Tlm Integer
              | Email String
              deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- (a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nome email agenda = agenda ++ [(nome,[Email email])]

-- (b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [(nom, cont)] = if nome == nom then Just (map (\x -> case x of Email e -> e) cont) else Nothing 
verEmails nome ((nom, cont):agenda) = if nome == nom then verEmails nome [(nom,cont)] else verEmails nome agenda 

-- (c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs (c:cs) = case c of Casa x -> x : consTelefs cs
                              Trab x -> x : consTelefs cs
                              Tlm x -> x : consTelefs cs
                              otherwise -> consTelefs cs

-- (d)
casa :: Nome -> Agenda -> Maybe Integer
casa nome [(nom, (c:cs))] = if nome == nom then case c of Casa x -> Just x 
                                                          otherwise -> casa nome [(nom,cs)]
                                           else Nothing  
casa nome ((nom,c):agenda) = if nome == nom then casa nome [(nom,c)] else casa nome agenda

-- 4.  
type Dia = Int
type Mes = Int
type Ano = Int
type Nome' = String

data Data = D Dia Mes Ano
          deriving Show
type TabDN = [(Nome,Data)]

-- (a)
procura :: Nome' -> TabDN -> Maybe Data
procura nome ((n,dat):tabDn) = if nome == n then Just dat else procura nome tabDn

-- (b)
idade :: Data -> Nome' -> TabDN -> Maybe Int
idade (D d1 m1 a1) nome ((nom,(D d2 m2 a2 )):t) | nome /= nom = idade (D' d1 m1 a1) nome t
                                                | a1 < a2 || (a1 == a2 && m1 < m2) = Just 0
                                                | m1 < m2 || (m1 == m2 && d1 < d2) = Just (a1-a2-1)
                                                | otherwise = Just (a1-a2)

-- (c)
anterior :: Data -> Data -> Bool
anterior (D d1 m1 a1) (D d2 m2 a2) | (a1<a2) || (a1==a2 && m1<m2) || (a1==a2 && m1==m2 && d1<d2) = True 
                                   | otherwise = False

-- (d)
ordena :: TabDN -> TabDN
ordena [] = []
ordena ((nom,dat):t) = aux (nom,dat)  (ordena t)
                       where aux (no,da) [] = [(no,da)]
                             aux (no,da) ((n,d):t) = if (anterior da d) == False then (n,d) : aux (no,da) t
                                                                                 else (no,da) : aux (n,d) t    

-- (e)
porIdade:: Data -> TabDN -> [(Nome,Int)]
porIdade _ [] = []
porIdade (D d1 m1 a1) tabDn = (nom, idade) : porIdade (D d1 m1 a1) tabDn
                              where ((nom,(D d' m' a')):t) = ordena tabDn
                                    idade = if (m1 < m' || (m1 == m' && d1 < d')) then (a1-a'-1) else (a1-a')

-- 5.  
data Movimento = Credito Float | Debito Float
               deriving Show
data Data'' = D'' Int Int Int
          deriving Show
data Extracto = Ext Float [(Data, String, Movimento)]
              deriving Show

-- (a)
extValor :: Extracto -> Float -> [Movimento]
extValor (Ext si []) val = []
extValor (Ext si ((_,_,Credito x):t)) val = if x > val then (Credito x) : extValor (Ext si t) val else extValor (Ext si t) val 
extValor (Ext si ((_,_,Debito x):t)) val = if x > val then (Debito x) : extValor (Ext si t) val else extValor (Ext si t) val 

-- (b)
filtro :: Extracto -> [String] -> [(Data,Movimento)]
filtro (Ext si []) _ = []
filtro (Ext si ((d,s,mov):t)) st = if elem s st then (d,mov) : filtro (Ext si t) st else  filtro (Ext si t) st

-- (c)
creDeb :: Extracto -> (Float,Float)
creDeb (Ext si resto) = (cred resto, deb resto)
                                where cred :: [(Data, String, Movimento)] -> Float
                                      cred [] = 0
                                      cred ((d,s,Credito x):t) = x + cred t
                                      cred ((d,s,Debito x):t) = cred t
                                      deb :: [(Data, String, Movimento)] -> Float
                                      deb [] = 0
                                      deb ((d,s,Debito x):t) = x + deb t
                                      deb ((d,s,Credito x):t) = deb t

-- (d) ???
saldo :: Extracto -> Float
saldo (Ext si resto) = si + c - d
                       where (c,d) = creDeb (Ext si resto)