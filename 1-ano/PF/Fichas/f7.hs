-- 1. 
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- (a)
calcula :: ExpInt -> Int
calcula (Const x) = x 
calcula (Simetrico x) = - calcula x
calcula (Mais x1 x2) = calcula x1 + calcula x2 
calcula (Menos x1 x2) = calcula x1 - calcula x2 
calcula (Mult x1 x2) = calcula x1 * calcula x2 

-- (b)
infixa :: ExpInt -> String
infixa (Const x) = show x 
infixa (Simetrico x) = "-" ++ "(" ++ infixa x ++ ")"
infixa (Mais x1 x2) = "(" ++ (infixa x1) ++ "+" ++ (infixa x2) ++ ")"
infixa (Menos x1 x2) = "(" ++ (infixa x1) ++ "-" ++ (infixa x2) ++ ")"
infixa (Mult x1 x2) = "(" ++ (infixa x1) ++ "*" ++ (infixa x2) ++ ")"

-- (c) 
posfixa :: ExpInt -> String
posfixa (Const x) = show x 
posfixa (Simetrico x) =  posfixa x ++ "-"
posfixa (Mais x1 x2) = (posfixa x1) ++ " " ++ (posfixa x2) ++ " +"
posfixa (Menos x1 x2) = (posfixa x1) ++ " " ++ (posfixa x2) ++ " -"
posfixa (Mult x1 x2) = (posfixa x1) ++ " " ++ (posfixa x2) ++ " *"

-- 2.  
data RTree a = R a [RTree a] -- rose trees
-- (a)
soma :: Num a => RTree a -> a
soma (R r []) = r 
soma (R r l) = r + sum (map soma l)

-- (b)
altura :: RTree a -> Int
altura (R r []) = 1 
altura (R r l) = 1 + maximum (map altura l)

-- (c) 
prune :: Int -> RTree a -> RTree a
prune 0 (R r _) =  R r []
prune x (R r []) | x > 1 = R r []
prune x (R r l) = R r (map (prune (x-1)) l)

-- (d)
mirror :: RTree a -> RTree a
mirror (R r l) = R r (map mirror (reverse l))

-- (e)
postorder :: RTree a -> [a]
postorder (R r l) = concat (map postorder l) ++ [r]

-- 3.  
data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a) -- leaf trees

-- (a)
ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e + ltSum d

-- (b)
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d 

-- (c)
ltHeight :: LTree a -> Int 
ltHeight (Tip x) = 1 
ltHeight (Fork e d) = 1+ max (ltHeight e)  (ltHeight d)

-- 4.  
data FTree a b = Leaf b | No a (FTree a b) (FTree a b) -- full trees

-- (a) 
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty,Tip x)
splitFTree (No r e d) = ( (Node r (fst(splitFTree e)) (fst(splitFTree d)) ) , (Fork (snd(splitFTree e)) (snd(splitFTree d)) ) )

-- (b) 
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip x) = Just (Leaf x)
joinTrees (Node r e1 d1) (Fork e2 d2) = Just (No r aux1 aux2)
                                            where Just aux1 = joinTrees e1 e2
                                                  Just aux2 = joinTrees d1 d2 
joinTrees _ _ = Nothing