import Data.Char

-- 1. 
-- (a)
funA :: [Double] -> Double
funA [] = 0
funA (y:ys) = y^2 + (funA ys)

{- A funA vai pegar individualmente no valor de cada elemento da lista e vai somando os quadrados de todos os elementos da lista.
Logo funA [2,3,5,1] = 4+9+25+1 = 39. -}

-- (b)
funB :: [Int] -> [Int]
funB [] = []
funB (h:t) = if (mod h 2)== 0 then h : (funB t)
             else (funB t)

{- A funB vai retirar os valores impares da lista e devolve uma lista com apenas os pares.
Logo funB [8,5,12] = [8,12]. -}

-- (c)
funC (x:y:t) = funC t
funC [x] = [x]
funC [] = []

{- -- A função funC vai dar a lista com o último elemento caso length da lista seja impar. Caso seja par o resultado será []
Logo funC [1,2,3,4,5] = [5]. -}

-- (d)
funD l = g [] l
g acc [] = acc
g acc (h:t) = g (h:acc) t

{- A funD vai fazer o reverse de uma string.
Logo funD "otrec" = "certo"-}

-- 2.
-- (a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = h * 2 : dobros t

-- (b)
numOcorre :: Char -> String -> Int
numOcorre x [] = 0
numOcorre x (h:t) = if x == h then 1 + numOcorre x t 
                    else numOcorre x t 

-- (c)
positivos :: [Int] -> Bool
positivos (h:t) = if h > 0 then positivos t 
                  else False

-- (d) 
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h > 0 then h : soPos t 
              else soPos t

-- (e)
somaNeg :: [Int] -> Int
somaNeg (h:t) = if h < 0 then h + somaNeg t
                else somaNeg t

-- (f)
tresUlt :: [a] -> [a]
tresUlt (h:t) = if (length (h:t)) <= 3 then (h:t) else tresUlt t

-- (g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,y):t) = [y] ++ segundos t 

-- (h) 
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros a ((x,y):t) = if a == x then nosPrimeiros a t
                         else False

-- (i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos ((a,b,c):t) =  (a+x,b+y,c+z)
                            where (x,y,z) = sumTriplos t

-- 3.  
-- (a) 
isDigit :: Char -> Bool
isDigit x = if ord(x) >= 48 && ord(x) <= 57 then True else False

soDigitos :: [Char] -> [Char]
soDigitos [] = []
soDigitos (h:t) = if Data.Char.isDigit h then h : soDigitos t else soDigitos t

-- (b)
minusculas :: [Char] -> Int
minusculas (h:t) = if elem h ['a'..'z'] then 1 + minusculas t else minusculas t

-- (c)
nums :: String -> [Int]
nums [] = []
nums (h:t) = if elem h ['0'..'9'] then (ord h - ord '0') : nums t else nums t

-- 4.  
type Polinomio = [Monomio]
type Monomio = (Float,Int)
-- (a) 
conta :: Int -> Polinomio -> Int
conta n ((x,y):t) = if n == y then 1 + conta n t else conta n t

-- (b)
grau :: Polinomio -> Int
grau [] = 0
grau [(x1,y1)] = y1
grau ((x1,y1):(x2,y2):t) = if y1 > y2 then grau ((x1,y1):t) else grau ((x2,y2):t)

-- (c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n ((x,y):t) = if n == y then (x,y) : selgrau n t
                      else selgrau n t

-- (d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((x,y):t) = if y > 0 then (x * (fromIntegral y), y-1) : deriv t else (x,y) : deriv t

-- (e) 
calcula :: Float -> Polinomio -> Float
calcula n [] = 0
calcula n ((x,y):t) = if y > 0 then (x * (n ^ y) + calcula n t) else calcula n t 

-- (f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) = if y == 0 then simp t else (x,y) : simp t

-- (g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (x,y) ((n,e):t) = (x*n,y*e) : mult (x,y) t

-- (h)
normaliza :: Polinomio -> Polinomio
normlaliza [] = []
normaliza ((x,y):t) = let lgi = selgrau y t
                          lgd = selgrau y t
                          s = sumRep ((x,y):lgi)
                      in if (s==0) then normaliza lgd else (s,y):(normaliza lgd)

sumRep :: Polinomio -> Float
sumRep [] = 0
sumRep ((x,y):t) = x + sumRep t

-- (i) 
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza(p1++p2)

-- (j)
produto :: Polinomio -> Polinomio -> Polinomio
produto [] _ = []
produto _ [] = []
produto (p1:t) p2 = mult p1 p2 ++ produto t p2

-- (k)
ordena :: Polinomio -> Polinomio
ordena [] = []
ordena ((n1,g1):t) = insertP (n1,g1) (ordena t)
                     where insertP :: Monomio -> Polinomio -> Polinomio
                           insertP (n1,g1) [] = [(n1,g1)]
                           insertP (n1,g1) ((n2,g2):t) = if g1 < g2 then (n1,g1):((n2,g2):t) else (n2,g2) : insertP (n1,g1) t

-- (l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena(normaliza p1) == ordena(normaliza p2)