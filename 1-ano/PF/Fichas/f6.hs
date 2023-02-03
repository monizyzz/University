-- 1.
data BTree a = Empty
             | Node a (BTree a) (BTree a)
           deriving Show

-- (a)
altura :: BTree a -> Int
altura Empty = 0 
altura (Node r e d) = 1 + max (altura e) (altura d)

-- (b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0 
contaNodos (Node r e d) = 1 + contaNodos e + contaNodos d

-- (c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) = folhas e + folhas d

-- (d)
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune n (Node r e d) = Node r (prune (n-1) e) (prune (n-1) d)

-- (e)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node r e d) = [r]
path (x:xs) (Node r e d) = if x == True then r : (path xs d) else r : (path xs e)

-- (f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

-- (g)
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = let e = (zipWithBT f e1 e2)
                                                  d = (zipWithBT f d1 d2)
                                              in Node (f r1 r2) e d
zipWithBT _ _ _ = Empty 

-- (h)
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty,Empty,Empty)
unzipBT (Node (r1,r2,r3) e d) = let (e1,e2,e3) = unzipBT e
                                    (d1,d2,d3) = unzipBT d 
                                in ((Node r1 e1 d1),(Node r2 e2 d2),(Node r3 e3 d3))

-- 2.  
-- (a)
minimo :: Ord a => BTree a -> a
minimo (Node r Empty _) = r
minimo (Node r e d) = minimo e

-- (b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty Empty) = Empty
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d 

-- (c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node r Empty Empty) = (r,Empty)
minSmin (Node r Empty d) = (r,d)
minSmin (Node r e d) = let (min, e') = minSmin e
                       in (min, Node r e' d)

-- (d) ???
remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty 
remove x (Node r e d) |  x > r = Node r e (remove x d)
                        | x < r = Node r (remove x e) d
                        | otherwise = aux d e
                        where aux :: Ord a => BTree a -> BTree a -> BTree a
                              aux Empty d = d
                              aux e Empty = e
                              aux e d = let ( m,d') = minSmin d
                                          in Node m e d'

-- 3. 
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
   deriving Show
type Turma = BTree Aluno --  ́arvore binária de procura (ordenada por número)

-- (a)
inscNum :: Numero -> Turma -> Bool
inscNum num Empty = False 
inscNum num (Node (n,_,_,_) e d) | num < n = inscNum num e
                                 | num > n = inscNum num d
                                 | otherwise = True

-- (b)
inscNome :: Nome -> Turma -> Bool
inscNome nom Empty = False 
inscNome nom (Node (_,n,_,_) e d) = nom == n || inscNome nom e || inscNome nom d

-- (c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,nome,reg,_) e d) = case reg of 
                                        TE -> [(num,nome)] ++ trabEst e ++ trabEst d
                                        _ -> trabEst e ++ trabEst d

-- (d)
nota :: Numero -> Turma -> Maybe Classificacao
nota n Empty = Nothing 
nota n (Node (num,nome,_,clas) e d) | n < num = nota n e
                                    | n > num = nota n d 
                                    | otherwise = Just clas 

-- (e)
percFaltas :: Turma -> Float
percFaltas Empty = 0 
percFaltas t = ((fromIntegral(faltas t))/(fromIntegral(contaNodos t)))*100
                                       where faltas :: Turma -> Int
                                             faltas Empty = 0
                                             faltas (Node (_,_,_,clas) e d) = case clas of Faltou -> 1 + faltas e + faltas d; otherwise -> 0

-- (f) ???
mediaAprov :: Turma -> Float
mediaAprov Empty = 0 
mediaAprov t = sumNotas t / total t
               where sumNotas :: Turma -> Float
                     sumNotas Empty = 0
                     sumNotas (Node (_,_,_,clas) e d) = case clas of Aprov nota -> fromIntegral nota + sumNotas e + sumNotas d; otherwise -> sumNotas e + sumNotas d
                     total :: Turma -> Float
                     total Empty = 0
                     total (Node (_,_,_,clas) e d) = case clas of Aprov nota -> 1 + total e + total d; otherwise -> 0

-- (g)
aprovAv :: Turma -> Float
aprovAv Empty = 0
aprovAv t = a/b
            where (a,b) = aux t
                  aux Empty = (0,0)
                  aux (Node (_,_,_,clas) e d) = case clas of Aprov nota -> (x+1,y); Rep -> (x,y+1); otherwise -> (x,y)
                                                   where (x,y) = (aprovesq + aprovdir, repesq + repdir)
                                                         (aprovesq,repesq) = aux e
                                                         (aprovdir,repdir) = aux d
{-
TRAVESSIAS
esq - pai - dir ===> infixa
esq - dir - pai ===> posfixa
pai - esq - dir ===> perfixa
-}