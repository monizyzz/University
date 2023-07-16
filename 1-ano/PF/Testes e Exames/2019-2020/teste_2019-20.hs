import Data.Char
import Data.List
import Data.Maybe

-- Teste 2019/2020

-- 1. Apresente uma definição recursiva da função (pré-definida):

-- (a) intersect :: Eq a => [a] -> [a] -> [a] que retorna a lista resultante de remover da primeira lista os elementos que não pertencem à segunda.

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (x:xs) l = if elem' x l then x:intersect' xs l
                      else intersect' xs l
                    where elem' _ [] = False 
                          elem' x (h:t) | x == h = True 
                                        | otherwise = elem' x t 

-- (b) tails :: [a] -> [[a]] que calcula a lista dos sufixos de uma lista.

tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = l : tails' (tail l)

{- 2. Para armazenar conjuntos de números inteiros, optou-se pelo uso de sequências de intervalos.
Assim, por exemplo, o conjunto {1, 2, 3, 4, 7, 8, 19, 21, 22, 23}
poderia ser representado por [(1,4),(7,8),(19,19),(21,23)]. -}

type ConjInt = [Intervalo]
type Intervalo = (Int,Int)

{- (a) Defina uma função elems :: ConjInt -> [Int] que, dado um conjunto, dá como resultado a lista
dos elementos desse conjunto. -}

elems :: ConjInt -> [Int]
elems [] = []
elems ((x,y):t) | x == y = x : elems t
                | otherwise = x : elems ((succ x , y):t)

{- (b) Defina uma função geraconj :: [Int] -> ConjInt que recebe uma lista de inteiros, ordenada por
ordem crescente e sem repetições, e gera um conjunto. -}

geraconj :: [Int] -> ConjInt
geraconj [] = []
geraconj l = let a = groupBy (\ x y -> x == y-1) l
                 b = map convInt a
             in b

convInt :: [Int] -> Intervalo
convInt l = (head l, last l)

{- 3. Para armazenar uma agenda de contactos telefónicos e de correio electrónico definiram-se os 
seguintes tipos de dados. Não existem nomes repetidos na agenda e para cada nome existe uma lista 
de contactos. -}

data Contacto = Casa Integer | Trab Integer | Tlm Integer | Email String deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contacto])]

{- (a) Defina a função acrescEmail :: Nome -> String -> Agenda -> Agenda que, dado um nome,
um email e uma agenda, acrescenta essa informação à agenda. -}

acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail nom em [] = [(nom,[Email em])]
acrescEmail nom em agenda = agenda ++ [(nom,[Email em])]

{- (b) Defina a função verEmails :: Nome -> Agenda -> Maybe [String] que, dado um nome e uma
agenda, retorna a lista dos emails associados a esse nome. Se esse nome não existir na agenda a
função deve retornar Nothing. -}

verEmails :: Nome -> Agenda -> Maybe [String]
verEmails nome [(nom,contacto)] = if nome == nom then Just (map (\x -> case x of Email e -> e) contacto) else Nothing
verEmails nome ((nom,contacto):agenda) = if nome == nom then verEmails nome [(nom,contacto)] else verEmails nome agenda

{- (c) Defina a função consulta :: [Contacto] -> ([Integer],[String]) que, dada lista de contactos, 
retorna o par com a lista de números de telefone (tanto telefones fixos como telemóveis) e a
lista de emails, dessa lista. Implemente a função de modo a fazer uma única travessia da lista de
contactos. -}

consulta :: [Contacto] -> ([Integer],[String])
consulta = foldr (\x (i,s) -> case x of Email email -> (i,email:s); otherwise -> (n x:i,s)) ([],[]) 
           where n x = case x of Casa num1 -> num1
                                 Trab num2 -> num2
                                 Tlm num3 -> num3

-- 4. Relembre o tipo RTree a definido nas aulas.

data RTree a = R a [RTree a] deriving (Show, Eq)

-- (a) Defina a função paths :: RTree a -> [[a]] que dada uma destas árvores calcula todos os caminhos desde a raíz até às folhas.

paths :: RTree a -> [[a]]
paths (R n []) = [[n]]
paths (R n ns) = map ((:) n . concat . paths) ns

{- (b) Defina a função unpaths :: Eq a => [[a]] -> RTree a inversa da anterior, i.e., tal que unpaths (paths t) == t, para qualquer 
árvore t :: Eq a => RTree a. -}

unpaths :: Eq a => [[a]] -> RTree a
unpaths [[h]] = R h []
unpaths [(h:t)] = R h [(unpaths [t])]
unpaths ((h:t):l) = R h ((unpaths [t]):(map unpaths [l]))