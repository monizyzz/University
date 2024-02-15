import Data.Char
import Data.List
import Data.Maybe

-- Teste 2018/2019

-- 1. Apresente uma definição recursiva da função (pré-definida) 

-- (a) elemIndices :: Eq a => a -> [a] -> [Int] que calcula a lista de posições em que um dado elemento ocorre numa lista.

elemIndices' :: Eq a => a -> [a] -> [Int] 
elemIndices' x [] = []
elemIndices' x l = aux 0 x l 
            where aux p x [] = [] 
                  aux p x (h:t) | x == h = p : aux (p+1) x t 
                                | otherwise = aux (p+1) x t

-- (b) isSubsequenceOf :: Eq a => [a] -> [a] -> Bool que testa se os elementos de uma lista ocorrem noutra pela mesma ordem relativa.

isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' l [] = False
isSubsequenceOf' [] l = True 
isSubsequenceOf' (x:xs) (y:ys) | x == y = isSubsequenceOf' xs ys
                               | otherwise = isSubsequenceOf' (x:xs) ys

-- 2. Considere o seguinte tipo para representar árvores binárias.

data BTree a = Empty | Node a (BTree a) (BTree a)

{- (a) Defina a funçãoo lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b que generaliza função
lookup para árvores binárias de procura. -}

lookupAP :: Ord a => a -> BTree (a,b) -> Maybe b
lookupAP x Empty = Nothing
lookupAP x (Node (r,b) e d) | x == r = Just b
                            | x > r = lookupAP x d 
                            | otherwise = lookupAP x e

{- (b) Defina a função zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c que generaliza a função zipWith para árvores binárias.-}

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT _ Empty _ = Empty
zipWithBT _ _ Empty = Empty
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)

{- 3. Defina a função digitAlpha :: String -> (String,String), que dada uma string, devolve um par de
strings: uma apenas com os números presentes nessa string, e a outra apenas com as letras presentes
na string. Implemente a função de modo a fazer uma única travessia da string. Sugestão: pode usar as
funções isDigit, isAlpha :: Char -> Bool. -}

digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (h:t) | isDigit h = (h:a,b)
                 | isAlpha h = (a,h:b)
                 where (a,b) = digitAlpha t


{-4. Considere o seguinte tipo de dados para representar uma sequência em que os elementos podem ser
acrecentados à esquerda (Cons) ou por concatenação de duas sequências (App). -}

data Seq a = Nil | Cons a (Seq a) | App (Seq a) (Seq a)

{- (a) Defina a função firstSeq :: Seq a -> a que recebe uma sequência não vazia e devolve o seu
primeiro elemento. -}

firstSeq :: Seq a -> a
firstSeq Nil = error "Something Wrong"
firstSeq (Cons a s) = a
firstSeq (App Nil s2) = firstSeq s2
firstSeq (App s1 _) = firstSeq s1

{- (b) Defina a função dropSeq :: Int -> Seq a -> Seq a, tal que dropSeq n s elimina os n primeiros
elementos da sequência s. A função deve manter a estrutura da sequência.
Por exemplo: dropSeq 2 (App (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil)) (Cons 1 Nil)) == App (Cons 3 Nil) (Cons 1 Nil) -}

dropSeq :: Int -> Seq a -> Seq a
dropSeq n Nil = Nil
dropSeq n (Cons a seq) = dropSeq (n-1) seq
dropSeq n (App a b) | n > nx = dropSeq (n - nx) b
                    | n == nx = b
                    | otherwise = App (dropSeq n a) b
                    where nx = contaCons a

contaCons :: Seq a -> Int
contaCons Nil = 0
contaCons (Cons _ seq) = 1 + contaCons seq
contaCons (App a b) = contaCons a + contaCons b

{- (c) Declare (Seq a) como instância da classe Show de forma a obter o seguinte comportamento no
interpretador:
> App (Cons 1 Nil) (App (Cons 7 (Cons 5 Nil)) (Cons 3 Nil))
<<1,7,5,3>>-}

instance Show a => Show (Seq a) where
    show x = "<<" ++ mostra x ++ ">>"

mostra :: Show a => Seq a -> String
mostra Nil = ""
mostra (Cons a Nil) = show a
mostra (Cons a s) = show a ++ "," ++ mostra s
mostra (App s1 s2) = mostra s1 ++ "," ++ mostra s2