-- 1.
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x > y = []
                | x == y = [x]
                | x < y = x : enumFromTo' (x+1) y

-- 2.
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | y > z = x : []
                      | x < z = x : enumFromThenTo' y (y + (y-x)) z
                      | otherwise = []

-- 3.
(+++) :: [a] -> [a] -> [a]
(+++) (h:t) l = h : (+++) t l
(+++) x [] = x
(+++) [] x = x

-- 4.
(!!!) :: [a] -> Int -> a
(!!!) (h:t) x | x == 0 = h 
              | x >= 1 = (!!!) t (x-1)

-- 5.
reversee :: [a] -> [a]
reversee (h:t) = reversee t ++ [h] 
reversee [] = []

-- 6. 
takee :: Int -> [a] -> [a]
takee n (h:t) | n == 0 = []
              | n >= 1 && n <= length t = h: takee (n-1) t
              | otherwise = (h:t)

-- 7.
dropp :: Int -> [a] -> [a]
dropp n (h:t) | n == 0 = (h:t)
              | n >= 1 && n <= length t = dropp (n-1) t
              | otherwise = []

-- 8.
zipp :: [a] -> [b] -> [(a,b)]
zipp (h1:t1) (h2:t2) = (h1,h2) :  zipp t1 t2
zipp _ _ = []

-- 9.
replicatee :: Int -> a -> [a]
replicatee 0 x = []
replicatee n x =  x : replicatee (n-1) x  

-- 10.
interspercee :: a -> [a] -> [a]
interspercee x [] = []
interspercee x [h] = [h]  
interspercee x (h:t) = h : x : interspercee x t

-- 11. +/-
takewhile' :: Eq a => a -> [a] -> [a]
takewhile' a [] = [a]
takewhile' a (h:t) | a == h = h : takewhile' a t
                   | otherwise = [a]

dropwhile' :: Eq a => a -> [a] -> [a]
dropwhile' a [] = []
dropwhile' a (h:t) | a == h = dropwhile' a t
                   | otherwise = h:t



group :: Eq a => [a] -> [[a]]
group [] = []
group (h:t) = takewhile' h t : group (dropwhile' h t)

-- or 
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (x:xs) = (takeWhile (==x)(x:xs)) : group' (dropWhile(==x)xs)
-- or
group'' :: Eq a => [a] -> [[a]]
group'' [] = []
group'' [x] = [[x]]
group'' (h:t) = if h == x then (h:x:xs):y
                else [h] : (x:xs):y
            where
                (x:xs):y = group t
-- 12.
concatt :: [[a]] -> [a]
concatt [] = []
concatt (h:t) = h ++ concatt t 

-- 13.
initss :: [a] -> [[a]]
initss [] = [[]]
initss l = initss (retiraUltimo l) ++ [l] 

retiraUltimo :: [a] -> [a]
retiraUltimo [x] = []
retiraUltimo (h:t) = h : retiraUltimo t

-- 14. 
tailss :: [a] -> [[a]]
tailss [] = [[]]
tailss l = [l] ++ tailss ( retiraPrimeiro l )

retiraPrimeiro :: [a] -> [a]
retiraPrimeiro [] = []
retiraPrimeiro (h:t) = t 

-- 15.
headss :: [[a]] -> [a]
headss [] = []
headss ([]:t) = headss t
headss ((x:xs):t) = x : headss t

-- 16. 
totals :: [[a]] -> Int
totals [] = 0 
totals ([]:t) = totals t
totals ((h:t):t2) = length (h:t) + totals t2 

-- 17. 
funs :: [(a,b,c)] -> [(a,c)]
funs [] = []
funs ((x,y,z):t) = (x,z) : funs t 

-- 18.
colas :: [(String,b,c)] -> String
colas [] = []
colas ((a,b,c):t) = a ++ colas t

-- 19. 
idades :: Int -> Int -> [(String,Int)] -> [String]
idades _ _ [] = []
idades a b ((x,y):t) = if a - y >= b then x : idades a b t
                       else idades a b t

-- 20. 
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n m = if m > 0 then powerEnumFrom' n (m-1) ++ [n ^(m-1)]
                     else []

-- 21. 
isPrime :: Int -> Bool
isPrime 1 = False 
isPrime n = aux (n-1) n 
    where aux i n | i == 1 = True 
                  | mod n i == 0 = False 
                  | otherwise = aux (i-1) n 

-- 22. 
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] l = True
isPrefixOf' (h1:t1) (h2:t2) = if h1 == h2 then isPrefixOf' t1 t2 
                              else False 

-- 23.  
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] [] = True 
isSuffixOf' l [] = False
isSuffixOf' (h:t) (x:xs) = if h == x && length t == length xs then isSuffixOf' t xs
                          else isSuffixOf' (h:t) xs

-- 24. 
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' l [] = False
isSubsequenceOf' (h:t) (x:xs) = if h == x then isSubsequenceOf' t xs 
                                else isSubsequenceOf' (h:t) xs

-- 25.
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' a (h:t) = if (a == h)
                       then 0 : [ y+1 | y <- elemIndices' a t ]
                       else [ y+1 | y <- elemIndices' a t]

elemIndices'' :: Eq a => a -> [a] -> [Int]
elemIndices'' _ [] = []
elemIndices'' n (h:t) = aux n (h:t) 0
    where aux n [] x = []
          aux n (h:t) x = if n == h then 0 : aux n t (x+1)
                          else aux n t (x+1) 
--aux :: Eq a => a -> [a] -> Int -> [Int]
--aux n [] x = []
--aux n (h:t) x = if n == h then 0 : aux n t (x+1)
                

-- 26.  +/-
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if elem h t then nub' t
            else nub' t ++ [h]
    where elem h [] = False 
          elem x (h:t) = if h == x then True 
                         else elem x t 

-- 27.  
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (h:t) = if n == h then t
                  else h : delete' n t

-- 28. 
(\\\):: Eq a => [a] -> [a] -> [a]
(\\\) [] l = []
(\\\) l [] = l
(\\\) (x:xs) (y:ys) = if x == y then (\\\) xs ys 
                      else x : (\\\) xs (y:ys)

-- or

barra :: Eq a => [a] -> [a] -> [a]
barra [] l = []
barra l [] = l
barra (x:xs) (h:t) = barra (delete1 h (x:xs)) t

delete1 :: Eq a => a -> [a] -> [a]
delete1 _ [] = []
delete1 x (h:t) = if x == h then t
                  else h: delete1 x t

-- 29. 
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l
union' l [] = l 
union' (x:xs) (y:ys) = if x == y then x : union' xs ys
                       else y : union' (x:xs) ys 

-- or 

myunion :: Eq a => [a] -> [a] -> [a]
myunion [] l = l 
myunion l [] = l
myunion (h:t) (x:xs) = if ocorre x (h:t) then myunion (h:t) xs
                       else myunion (h:t) xs ++ [x]

ocorre :: Eq a => a -> [a] -> Bool 
ocorre _ [] = False 
ocorre n (h:t) = if n == h then True 
                 else ocorre n t 

-- 30.
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' l [] = []
intersect' (x:xs) (y:ys) = if ocorre x (y:ys) then x : intersect' xs (y:ys)
                           else intersect' xs (y:ys)

-- 31.  
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (h:t) | n <= h = n : insert' h  t
                | otherwise = h : insert' n t

-- 32. 
unwords' :: [String] -> String
unwords' [] = []
unwords' [x] = x
unwords' (x:y) = x ++ " " ++ unwords' y

-- 33. 
unlines' :: [String] -> String
unlines' [] = []
unlines' [x] = x ++ "\n"
unlines' (x:y) = x ++ "\n" ++ unlines' y

-- 34. 
pMaior' :: Ord a => [a] -> Int
pMaior' (h:t) = posicaoM h t 0 1

posicaoM :: Ord a => a -> [a] -> Int -> Int -> Int
posicaoM _ [] x _ = x
posicaoM m (h:t) x n = if h > m then posicaoM h t n (n+1)
                   else posicaoM m t x (n+1)

-- 35. 
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' a ((x,y):t) = if a == x then Just y
                       else lookup' a t

-- 36.  
preCrescente' :: Ord a => [a] -> [a]
preCrescente' [] = []
preCrescente' [x] = [x]
preCrescente' (x:y:t) = if x <= y then x : preCrescente' (y:t)
                      else [x]

-- 37. 
iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' [x] = [x]
iSort' (h:t) = insert' h (iSort' t)

insert'' :: Ord a => a -> [a] -> [a]
insert'' a [] = [a]
insert'' a (h:t) = if a <= h then a:h:t
                  else h : insert' a t

-- 38.
menor' :: String -> String -> Bool
menor' xs ys = if (length xs) < (length ys) then True
                       else False

-- 39.  
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' a ((x,y):t) = if a == x then True 
                        else elemMSet' a t 

-- 40. 
converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,y):t) = if y > 0 then x : converteMSet' ((x,y-1):t)
                          else converteMSet' t

-- 41.  
insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' a [] = [(a,1)]
insereMSet' a ((x,y):t) = if a == x then ((x,y+1):t)
                          else (x,y) : insereMSet' a t 

-- 42.  
removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' a [] = []
removeMSet' a ((x,y):t) | a == x && y == 1 = t
                        | a == x = (x,y-1):t
                        | otherwise = (x,y) : removeMSet' a t 

-- 43.  
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' [x] = [(x,1)]
constroiMSet' (h:t) = insereMSet'' h (constroiMSet' t)
              where 
                insereMSet'' a [] = [(a,1)]
                insereMSet'' a ((x,y):t) = if a == x then ((x,y+1):t)
                                          else (x,y) : insereMSet' a t 
-- or
constroiMSet'' :: Ord a => [a] -> [(a,Int)]
constroiMSet'' [] = []
constroiMSet'' (x:xs) = (x, quantidade x (x:xs)) : constroiMSet'' (dropWhile (==x) xs)

quantidade :: Eq a => a -> [a] -> Int 
quantidade a [] = 0 
quantidade a (x:xs) = if a == x then 1 + quantidade a xs else 0 + quantidade a xs

-- 44. 
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' (h:t) = case h of 
        Left   a -> ( a:x , y )
        Right b -> ( x , b:y )
    where (x,y) = partitionEithers' t

-- 45.  ???
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = [] 
catMaybes' (h:t) = case h of
        Just a -> a : catMaybes' t
        Nothing -> catMaybes' t

-- 46. ???
data Movimento = Norte | Sul | Este | Oeste
               deriving Show

caminho' :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho' (x,y) (x2,y2) | x > x2 = Oeste : caminho' (x-1,y) (x2,y2)
                       | x < x2 = Este : caminho' (x+1, y) (x2,y2)
                       | y > y2 = Sul : caminho' (x, y-1) (x2,y2)
                       | y < y2 = Norte : caminho' (x, y+1) (x2,y2)
                       | otherwise = []

-- 47.  ???
{- hasLoops :: (Int, Int) -> [Movimento] -> Bool
hasLoops (x,y) [] = False
hasLoops (x,y) (h:t) = case h of
    Norte -> aux'' (x,y) (x,y+1) t
    Sul -> aux'' (x,y) (x,y-1) t
    Este -> aux'' (x,y) (x+1,y) t
    Oeste -> aux'' (x,y) (x-1, y) t 

aux'' :: (Int, Int) -> (Int, Int) -> [Movimento] -> Bool
-- (xi,yi) x inicial y inicial   ...  (xa,ya) x atual y atual
aux'' (xi, yi) (xa, ya) [] = if xi == xa && yi == ya then True
                             else False
aux'' (xi, yi) (xa,ya) (h:t) = if xi == xa && yi == ya then True
                               else case h of
                                   Norte -> aux'' (xi,yi) (xa,ya+1) t
                                   Sul -> aux'' (xi,yi) (xa,ya-1) t
                                   Oeste -> aux'' (xi,yi) (xa-1,ya) t
                                   Este -> aux'' (xi,yi) (xa+1, ya) t
-}

-- 48. 
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int 
contaQuadrados [] = 0
contaQuadrados ((Rect (x1,y1) (x2,y2)):t) = if abs(x1-x2) == abs(y1-y2) then 1 + contaQuadrados t
                                            else contaQuadrados t

-- 49.  
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs(x1-x2) * abs(y1-y2) + areaTotal t

-- 50. 
data Equipamento = Bom | Razoavel | Avariado
                 deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) = case h of
    Avariado -> naoReparar t
    _ -> 1 + naoReparar t

-- End

