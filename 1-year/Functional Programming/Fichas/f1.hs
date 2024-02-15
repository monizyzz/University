import Data.List
import Data.Char

-- 1. 
-- (a)
perimetro :: Float -> Float 
perimetro r = 2 * pi * r

-- (b)
dist :: (Double,Double) -> (Double,Double) -> Double 
dist (x1,y1) (x2,y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- (c)
primUlt :: [Int] -> (Int,Int)
primUlt l = (head l, last l)

-- (d)
multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n == 0 then True
               else False

-- (e)
truncaImpar :: [Int] -> [Int]
truncaImpar (h:t) | mod (length (h:t)) 2 == 0 = (h:t) 
                  | otherwise = t

-- (f)
max2 :: Int -> Int -> Int
max2 x y | x > y = x 
         | x < y = y

-- (g)
max3 :: Int -> Int -> Int -> Int
max3 x y z | (max2 x y) > z = max2 x y
           | otherwise = z

-- 2.
-- (a)
nRaizes :: Float -> Float -> Float -> Float 
nRaizes a b c | delta1 < 0 = 0
              | delta1 == 0 = 1 
              | delta1 > 0 = 2
              where delta1 = (b^2)-4*a*c

-- (b)
nRaizes2 :: Float->Float->Float->[Float]
nRaizes2 x y z | delta2 < 0 = []
               | delta2 == 0 = [r]
               | delta2 > 0 = [r1,r2]
               where
                delta2 = (y^2-4*x*z) 
                r= (-y)/(2*x)
                r1= ((-y)+sqrt delta2)/(2*x)
                r2= ((-y)-sqrt delta2)/(2*x)

-- 3. 
type Hora = (Int,Int) 
-- (a)
horaValida :: Hora -> Bool 
horaValida (h,m) | h < 24 && h >= 0 && m < 60 && m >= 0 = True
                 | otherwise = False 

-- (b)
compHora :: Hora -> Hora -> Bool 
compHora (h1,m1) (h2,m2) | h1 > h2 = True
                         | h1 == h2 && m1 > m2 = True
                         | otherwise = False

-- (c)
convH:: Hora -> Int 
convH (h,m) = h * 60 + m

-- (d)
convM  :: Int -> Hora 
convM m = (div m 60,mod m 60)

-- (e)
difH :: Hora -> Hora -> Int 
difH (h1,m1) (h2,m2) = (abs(h1-h2)*60) + abs(m1-m2)

-- (f)
sumH :: Int -> Hora -> Hora
sumH x (h,m) = (h + (div (x+m) 60), mod (x+m) 60)

-- 4.
data Hora' = H Int Int 
            deriving (Show,Eq)

hValida' :: Hora' -> Bool
hValida' (H x y) = if x < 24 && y < 60 then True else False

hDepois' :: Hora' -> Hora' -> Bool
hDepois' (H x1 y1) (H x2 y2) | x1 > x2 = True
                             | x1 < x2 = False
                             | x1 == x2 && y1 > y2 = True
                             |otherwise = False  

convH' :: Hora' -> Int
convH' (H x y) = x*60 + y

convM' :: Int -> Hora'
convM' h = (H (div h 60)  (mod h 60)) 

difM' :: Hora' -> Hora' -> Int
difM' (H x1 y1) (H x2 y2) = (abs(x1-x2)*60) + abs(y1-y2)

sumH' :: Int -> Hora' -> Hora'
sumH' x (H h1 m1) = (H (h1 + (div (x+m1) 60))  (mod (x+m1) 60))

-- 5.  
data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- (a)
next :: Semaforo -> Semaforo
next x | x == Verde = Amarelo
       | x == Amarelo = Vermelho
       | x == Vermelho = Verde

-- (b)
stop :: Semaforo -> Bool
stop x | x == Vermelho = True 
       | otherwise = False 

-- (c)
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = if s1 == Vermelho || s2 == Vermelho then False else True

-- 6.  
data Ponto = Cartesiano Double Double
             deriving (Show,Eq)

-- (a)
posx :: Ponto -> Double
posx (Cartesiano x1 y1) = x1

-- (b)
posy :: Ponto -> Double
posy (Cartesiano x1 y1) = y1

-- (c)
raio :: Ponto -> Double
raio (Cartesiano x1 y1) = sqrt (x1 ^ 2 + y1 ^ 2)

-- (d)
angulo :: Ponto -> Double
angulo (Cartesiano x1 y1) |x1==0 && y1==0 =0
                          |x1==0 && y1>0 = pi/2
                          |x1==0 && y1<0 = -pi/2
                          |x1>0 = atan (y1/x1)
                          |x1<0 && y1>=0 = atan (y1/x1) + pi
                          |x1<0 && y1<0 = atan (y1/x1) - pi

-- (e)
dist' :: Ponto -> Ponto -> Double
dist' (Cartesiano x1 y1) (Cartesiano x2 y2) = sqrt ((x2-x1)^2 + (y2-y1)^2)

-- 7. 
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
              deriving (Show,Eq)

-- (a)
poligono :: Figura -> Bool
poligono (Circulo a r) = if r > 0 then True else False
poligono (Rectangulo a b) = if (posx a) /= (posx b) && (posy a) /= (posy b) then True else False
poligono (Triangulo a b c) = ((x+y)>z) || ((x+z)>y) || ((y+z)>x)
                           where 
                           x = dist' a b
                           y = dist' a c
                           z = dist' b c

-- (b)
vertices :: Figura -> [Ponto]
vertices (Rectangulo a b) = [Cartesiano x1 y1, Cartesiano x1 y2, Cartesiano x2 y1, Cartesiano x2 y2]
                            where 
                            x1 = (posx a)
                            x2 = (posx b)
                            y1 = (posy a)
                            y2 = (posy b)
vertices (Triangulo a b c) = [a,b,c]

-- (c)
area :: Figura -> Double
area (Triangulo p1 p2 p3) =
       let a = dist' p1 p2
           b = dist' p2 p3
           c = dist' p3 p1
           s = (a+b+c) / 2 -- semi-perimetro
       in sqrt (s*(s-a)*(s-b)*(s-c)) -- formula de Heron
area (Circulo a r) = pi * (r) ^ 2
area (Rectangulo a b) = if posx a > posy b then (posx a - posx b) * (posy a - posy b) else (posx b - posx a) * (posy b - posy a)  

-- (d)
perimetro' :: Figura -> Double
perimetro' (Circulo a r) = 2 * pi * r
perimetro' (Rectangulo a b) = 2 * (((abs (posx a)) + (abs(posx b))) + ((abs (posy a)) + (abs(posy b))))
perimetro' (Triangulo a b c) = let p1 = dist' a b 
                                   p2 = dist' b c 
                                   p3 = dist' c a
                               in p1 + p2 + p3
                  
-- 8.  
-- (a)
isLower' :: Char -> Bool
isLower' x = if ord(x) >= 95 && ord(x) <= 122 then True else False

-- (b)
isDigit' :: Char -> Bool
isDigit' x = if elem x ['0'..'9'] then True else False

-- (c)
isAlpha' :: Char -> Bool
isAlpha' x = if isLower x || elem x ['A'..'Z'] then True else False

-- (d)
toUpper' :: Char -> Char
toUpper' x = if isLower x == True then chr (ord(x) - 32) else x

-- (e)
intToDigit' :: Int -> Char
intToDigit' x = if elem x [0..9] then chr( ord('0') + x) else error ""

-- (f)
digitToInt' :: Char -> Int
digitToInt' x = if isAlpha x == True then (ord x - ord '0') else error ""