import Data.Char
import Data.List
import Data.Maybe

-- Teste 2017/2018

{-1. Apresente uma definição recursiva da função (pré-definida) insert :: Ord a => a -> [a] -> [a] 
que dado um elemento e uma lista ordenada retorna a lista resultante de inserir ordenadamente esse elemento na lista.-}

insert' :: Ord a => a -> [a] -> [a] 
insert' x [] = [x]
insert' x (h:t) | x <= h = x : h : t
                | otherwise = h : insert' x t 

{-2. Apresente uma definição recursiva da função pré-definida catMaybes :: [Maybe a] -> [a]
que colecciona os elementos do tipo a de uma lista. -}

catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' (h:t) = case h of 
            Nothing -> catMaybes' t
            Just x -> x : catMaybes' t

{-3. Considere o tipo ao lado para representar expressões aritméticas com variáveis. Defina Exp a como 
instância da classe Show, de forma a que show (Mais (Var "x") (Mult (Const 3) (Const 4))) seja 
a string "(x + (3 * 4))". -}

data Exp a = Const a | Var String | Mais (Exp a) (Exp a) | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where 
    show (Const x) = show x
    show (Var x) = x
    show (Mais x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Mult x y) = "(" ++ show x ++ " * " ++ show y ++ ")" 

{- 4. Apresente uma definição da função sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena
uma lista comparando os resultados de aplicar uma função de extração de uma chave a cada elemento de uma lista. 
Por exemplo: sortOn fst [(3,1),(1,2),(2,5)] == [(1,2),(2,5),(3,1)]. -}

sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f (h:t) = aux f h (sortOn' f t)
                  where aux :: Ord b => (a -> b) -> a -> [a] -> [a] 
                        aux f x [] = [x]
                        aux f x (h:t) = if f x > f h then h : aux f x t
                                        else x : h : t

{- 5. A amplitude de uma lista de inteiros define-se como a diferença entre o maior e o menor dos elementos
da lista (a amplitude de uma lista vazia é 0).

(a) Defina a função amplitude :: [Int] -> Int que calcula a amplitude de uma lista (idealmente numa única passagem pela lista). -}

amplitude :: [Int] -> Int
amplitude [] = 0
amplitude l = maior - menor
              where (maior,menor) = foldr (\v (a,b) -> (if v > a then v else a, if v < b then v else b)) (head l, head l) l 

{- (b) Defina a função parte :: [Int] -> ([Int],[Int]) que parte uma lista de inteiros em duas, minimizando a soma das amplitudes. 
Por exemplo, parte [1,18,3,19,17,20] deve colocar 1 e 3 numa das listas e os restantes na outra. Admita, caso necessite, que existe 
pré-definida uma função sort :: Ord a => [a] -> [a] de ordenação de listas. -}

parte :: [Int] -> ([Int],[Int])
parte [] = ([],[])
parte l = let ordenada = sort l
              metade = div (last ordenada - head ordenada) 2
              metInf = filter(\z ->last ordenada - z <= metade ) ordenada
              metSup = filter(\z ->last ordenada - z > metade) ordenada
          in (metInf,metSup)

--6. Considere o seguinte tipo para representar imagens compostas por quadrados (apenas com coordenadas positivas)

data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]

{- Por exemplo, a seguinte imagem é constituída por três quadrados.

ex :: Imagem
ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])

(a) Defina a função conta :: Imagem -> Int que conta quantos quadrados tem uma imagem. -}

conta :: Imagem -> Int
conta (Quadrado a) = 1
conta (Mover (_,_) a) = conta a
conta (Juntar l) = sum (map conta l)