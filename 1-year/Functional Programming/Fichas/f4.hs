
import Data.Char 

-- 1. 
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (a,h:b)
                 | isAlpha h = (h:a,b)
                 | otherwise = digitAlpha t
                  where (a,b) = digitAlpha t

digitAlpha' :: String -> (String,String)
digitAlpha' string = foldl (\(alpha,digit) s -> if isDigit s then (alpha,digit ++ [s]) else if isAlpha s then (alpha ++ [s], digit) else (alpha,digit)) ("","") string

-- 2.
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h == 0 = (a,b+1,c)
          | h < 0 = (a+1,b,c)
          | h > 0 = (a,b,c+1)
          | otherwise = nzp t
          where (a,b,c) = nzp t 

nzp' :: [Int] -> (Int,Int,Int)
nzp' = foldl (\(n,z,p) x -> if x > 0 then (n,z,p+1) else if x == 0 then (n,z+1,p) else (n+1,z,p)) (0,0,0)

-- 3.  ???
divMod' :: Integral a => a -> a -> (a, a)
divMod' x y = if x - y > 0 then (divi+1,res) else (0,x)
              where (divi,res) = divMod' (x-y) y

divMod'' :: Integral a => a -> a -> (a, a)
divMod'' x y = foldl (\(div',res) n -> (div'+1,res-y)) (0,x) [y,2*y..x]

-- 4.  ???
fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t  

fromDigits' :: [Int] -> Int
fromDigits' l = aux 0 l 
                where aux :: Int -> [Int] -> Int 
                      aux n [] = n
                      aux n (h:t) = aux (n*10+h) t

fromDigits'':: [Int] -> Int
fromDigits'' = foldl (\acc x -> x*10^acc) 0 

-- 5.  ???
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l = maximum [sum m | m <- initss l]

maxSumInit' :: (Num a, Ord a) => [a] -> a
maxSumInit' l = foldl (\acc x -> max (sum x) acc) (sum l) (initss l)

initss :: [a] -> [[a]]
initss [] = [[]]
initss l = initss (retiraUltimo l) ++ [l] 

retiraUltimo :: [a] -> [a]
retiraUltimo [x] = []
retiraUltimo (h:t) = h : retiraUltimo t

-- 6.  ???
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib' :: Int -> Int
fib' n = aux (0,1) n 
         where aux :: (Int,Int) -> Int -> Int 
               aux (a,b) 0 = a 
               aux (a,b) 1 = b
               aux (a,b) x | x>1 = aux (b,a+b) (x-1)

-- 7.  ???
intToStr' :: Int -> String -> String
intToStr' x acc | x < 0     = '-' : intToStr' (abs x) acc
                | x == 0    = acc
                | otherwise = intToStr' (x `div` 10) (chr (x `mod` 10 + ord '0') : acc)

-- 8.  
-- (a) [x | x <- [1..20], mod x 2 == 0, mod x 3 == 0]

-- Pega no x que varia de 1 até 20 retirando apenas aqueles que têm modulo tanto de 2 como 3 ficando [6,12,18]  

-- (b) [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]

-- Faz exatamente o mesmo que a alínea anterior logo fica [6,12,18]

-- (c) [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]

-- Vai retornar uma lista de pares tal que a soma dê 30. Ficando[(10,20),(11,19),(12,18),(13,17),...,(20,10)]

-- (d) [sum [y | y <- [1..x], odd y] | x <- [1..10]]

{-Neste caso y apenas pode tomar valores impares de x e acaba por formar uma lista com os seguintes valores [[1],[1],[1,3],[1,3],[1,3,5],
[1,3,5],[1,3,5,7],[1,3,5,7],[1,3,5,7,9],[1,3,5,7,9]], por fim faz a soma dos valores de cada lista ficando [1,1,4,4,9,9,16,16,25,25] -}

-- 9.  
-- (a)
-- Esta lista representa os valores de 2^0 ate 2^10.
exp2 = [2^x | x <- [0..10]]

-- (b)
-- Par de valores em que a soma dê 6.
sum6 = [(x,y) | x <- [1..5], y <- [1..5], x+y == 6]

-- (c)
-- Lista com listas de todos os valores menores que 6.
min6 = [[y | y <- [1..x]] | x <- [1..5]]
min6' = [[1..x] | x <- [1..5]]

-- (d)
-- Lista com listas de o valor 1 repetidos no máximo 5 vezes
list1 = [replicate x 1 | x <- [1..5]]

-- (e)
--720/120 = 6; 120/24 = 5; 24/6 = 4;...
prod = [product [y | y <- [1..x]] | x <-[1..6]]