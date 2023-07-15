import Data.Char
import Data.List
import Data.Maybe
import Distribution.Simple (coverageSupported)


-- Teste 2016/2017

--1. Considere o tipo MSet a para representar multi-conjuntos de tipo a

type MSet a = [(a,Int)]

{- Considere ainda que nestas listas não há pares cuja primeira componente coincida, nem cuja segunda
componente seja menor ou igual a zero. Para além disso, os multi-conjuntos estão organizados
por ordem decrescente da muiltplicidade. O multi-conjunto {’b’,’a’,’c’,’b’,’b’,’a’,’b’} é
representado pela lista [(’b’,4),(’a’,2),(’c’,1)], por exemplo. -}

{-(a) Defina a função cardMSet :: MSet a -> Int que calcula a cardinalidade de um multiconjunto. 
Por exemplo, cardMSet [(’b’,4),(’a’,2),(’c’,1)] devolve 7. -}

cardMSet :: MSet a -> Int
cardMSet [] = 0
cardMSet ((x,y):t) = y + cardMSet t 

-- cardMset  = foldr( (+) . snd) 0

{- (b) Defina a função moda :: MSet a -> [a] que devolve a lista dos elementos com maior número
de ocorrências. -}

moda :: MSet a -> [a]
moda [] = []
moda ((x,y):t) = aux (x,y) t 
        where 
              aux (x,y) [] = [x]
              aux (x,y) ((a,b):t) | y > b = aux (x,y) t 
                                  | y == b = x : aux (a,b) t
                                  | otherwise = aux (a,b) t 

{- (c) Defina a função converteMSet :: MSet a -> [a] que converte um multi-conjunto numa
lista. Por exemplo, converteMSet [(’b’,4),(’a’,2),(’c’,1)] devolve ‘‘bbbbaac’’. -}

converteMSet :: MSet a -> [a]
converteMSet [] = []
converteMSet [(x,1)] = [x]
converteMSet ((x,y):t) = if y > 0 then x : converteMSet ((x,y-1):t)
                         else converteMSet t
 
{- (d) Defina a função addNcopies :: Eq a => MSet a -> a -> Int -> MSet a que faz a inserção
de um dado número de ocorrências de um elemento no multi-conjunto, mantendo a ordenação
por ordem decrescente da multiplicidade. Não use uma função de ordenação. -}

addNcopies :: Eq a => MSet a -> a -> Int -> MSet a
addNcopies [] a n = [(a,n)]
addNcopies ((x,y):t) a n | n >= y = (a,n) : (x,y) : t
                         | otherwise = (x,y) : addNcopies t a n

--2. Considere o seguinte tipo de dados para representar subconjuntos de números reais (Doubles).

data SReais = AA Double Double | FF Double Double | AF Double Double | FA Double Double | Uniao SReais SReais

{- (AA x y) representa o intervalo aberto ]x, y[, (FF x y) representa o intervalo fechado [x, y], (AF x
y) representa ]x, y], (FA x y) representa [x, y[ e (Uniao a b) a união de conjuntos. -}

{- (a) Defina a SReais como instância da classe Show, de forma a que, por exemplo, a apresentação
do termo Uniao (Uniao (AA 4.2 5.5) (AF 3.1 7.0)) (FF (-12.3) 30.0) seja
((]4.2,5.5[ U ]3.1,7.0]) U [-12.3,30.0]) -}

instance Show SReais where 
      show (AA x y) = "]" ++ show x ++ "," ++ show y ++ "["
      show (FF x y) = "[" ++ show x ++ "," ++ show y ++ "]"
      show (AF x y) = "]" ++ show x ++ "," ++ show y ++ "]" 
      show (FA x y) = "[" ++ show x ++ "," ++ show y ++ "["
      show (Uniao x y) = "(" ++ show x ++ " U " ++ show y ++ ")"

{- (b) Defina a função pertence :: Double -> SReais -> Bool que testa se um elemento pertence
a um conjunto. -}

pertence :: Double -> SReais -> Bool
pertence x (AA a b) = x > a && x < b
pertence x (FF a b) = x >= a && x <= b
pertence x (AF a b) = x > a && x <= b 
pertence x (FA a b) = x >= a && x < b 
pertence x (Uniao a b) = pertence x a && pertence x b

--(c) Defina a função tira :: Double -> SReais -> SReais que retira um elemento de um conjunto.

tira :: Double -> SReais -> SReais
tira x (Uniao a b) = Uniao (tira x a) (tira x b)

tira x (AA a b) | pertence x (AA a b) = Uniao (AA a x) (AA x b)
                | otherwise = AA a b

tira x (FF a b) | pertence x (FF a b) = Uniao (FA a x) (AF x b)
                | otherwise = FF a b

tira x (AF a b) | pertence x (AF a b) = Uniao (AA a x) (AF x b)
                | otherwise = AF a b

tira x (FA a b) | pertence x (FA a b) = Uniao (FA a x) (AA x b)
                | otherwise = FA a b

--3. Considere o seguinte tipo para representar árvores irregulares (rose trees).

data RTree a = R a [RTree a]

{- (a) Defina a função percorre :: [Int] -> RTree a -> Maybe [a] que recebe um caminho e
uma árvore e dá a lista de valores por onde esse caminho passa. Se o caminho não for válido
a função deve retornar Nothing. O caminho é representado por uma lista de inteiros (1 indica
seguir pela primeira sub-árvore, 2 pela segunda, etc). -} 

percorre :: [Int] -> RTree a -> Maybe [a]
percorre [] (R a _) = Just [a]
percorre _  (R a []) = Nothing
percorre (x:xs)  (R a l) = Just (a:b)
                           where b = aux (percorre xs (l!!(x-1)))
                                 aux Nothing = []
                                 aux (Just y) = y 