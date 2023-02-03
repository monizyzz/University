{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

-- 1. 
-- (a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False 
any' f (h:t) = f h || any' f t

-- (b)
zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' f _ _= []

-- (c)
takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if f x then x : takeWhile' f xs
                      else []

-- (d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) = if f x then dropWhile' f xs 
                      else (x:xs)

-- (e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' f [] = ([],[])
span' f (x:xs) | f x = (x:a,b)
               | otherwise = ([],x:xs)
               where (a,b) = span' f xs

-- (f) 
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f _ [] = [] 
deleteBy' f n (x:xs) | f n x = xs 
                     | otherwise = x : deleteBy' f n xs

-- (g)
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = [] 
sortOn' f (h:t) = aux f h (sortOn' f t)
                  where aux :: Ord b => (a -> b) -> a -> [a] -> [a]
                        aux f x [] = [x]
                        aux f x (a:b) = if f x > f a then a : aux f x b else x : a : b

-- 2.  
type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- (a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n pol  = filter (\x -> n == snd (x)) pol

-- (b)
conta :: Int -> Polinomio -> Int
conta n pol = foldr (\x conta -> if n == snd(x) then conta+1 else conta) 0 pol

-- (c)
grau :: Polinomio -> Int
grau pol = foldr (\x g -> if snd x > g then snd(x) else g) 0 pol 

-- (d)
deriv :: Polinomio -> Polinomio
deriv pol = let l = map (\(c,g) -> if g > 0 then (c*fromIntegral(g),g-1) else (0,0)) pol
             in filter(/=(0,0)) l

-- (e)
calcula :: Float -> Polinomio -> Float
calcula n pol = foldr (\(c,g) soma -> c*(n^g) + soma ) 0 pol

-- (f)
simp :: Polinomio -> Polinomio
simp pol = filter (\x -> fst(x) /= 0) pol

-- (g)
mult :: Monomio -> Polinomio -> Polinomio
mult (cm,gm) pol = map (\x -> (fst(x)*cm,snd(x)*gm)) pol 

-- (h)
ordena :: Polinomio -> Polinomio
ordena pol = foldr aux [] pol
             where aux :: Monomio -> Polinomio -> Polinomio
                   aux (cm,gm) [] = [(cm,gm)]
                   aux (cm,gm) ((cm2,gm2):t) = if gm < gm2 then (cm,gm): (cm2,gm2): t else (cm2,gm2): aux (cm,gm) t

ordena' :: Polinomio -> Polinomio
ordena' pol = sortOn' snd pol

-- (i) ???
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza ((b,e):ps) = (sum [bs | (bs,es) <- selgrau e ps] + b,e):normaliza [(bo,eo) | (bo,eo) <- ps, eo /= e]

-- (j)
soma' :: Polinomio -> Polinomio -> Polinomio
soma' pol1 pol2 = normaliza (pol1 ++ pol2)

-- (k)
produto :: Polinomio -> Polinomio -> Polinomio
produto pol1 pol2 = foldr mult pol1 pol2

-- (l) 
equiv :: Polinomio -> Polinomio -> Bool
equiv pol1 pol2 = ordena(normaliza pol1) == ordena(normaliza pol2)

-- 3.  
type Mat a = [[a]]
{- matriz (triangular superior)
|1 2 3|
|0 4 5|   [[1,2,3], [0,4,5], [0,0,6]]
|0 0 6|
-}
-- (a)
dimOK :: Mat a -> Bool
dimOK (l:rm) = all (\l1 -> length l == length l1) rm

-- (b)
dimMat :: Mat a -> (Int,Int)
dimMat (l:rm) = (length l, length (l:rm))

-- (c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat m1 m2 = zipWith' (\l1 l2 -> zipWith' (+) l1 l2) m1 m2

-- (d) ???
transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m = let l = map head m
                  rm = map tail m
              in l: transpose rm

-- (e)
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = zipWith (\l1 l2 -> zipWith (*) l1 l2) m1 m2

-- (f) ???
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f m1 m2 = zipWith (\l1 l2 -> zipWith f l1 l2) m1 m2

-- (g) ???
triSup :: (Num a,Eq a) => Mat a -> Bool
triSup [] = True
triSup (h:t) = let l = map head t
                   rm = map tail t
               in all (==0) l && triSup rm

-- (h)
rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = [[]]
rotateLeft m = let l = map last m
                   rm = map init m
               in l: rotateLeft rm