-- Exame 2016/2017
import Data.Char
import Data.List 
{- 1. Apresente uma definição recursiva das seguintes funções (pré-definidas) sobre listas:
(a) unlines :: [String] -> String que junta todas as strings da lista numa só, separando-as
pelo caracter ’\n’. Por exemplo, unlines ["Prog", "Func" == "Prog\nFunc". -} 

unlines' :: [String] -> String
unlines' [] = ""
unlines' [x] = x
unlines' (h:t) = h ++ "\n" ++ unlines' t 

{- (b) (\\) :: (Eq a) => [a] -> [a] -> [a] que retorna a lista resultante de remover (as primeiras
ocorrências) dos elementos da segunda lista da primeira. Por exemplo,
(\\) [1,2,3,4,5,1] [1,5] == [2,3,4,1] e (\\) [1,2,2,3,2,1,4,1] [2,1,2] == [3,2,1,4,1]. -}

(\\\) :: (Eq a) => [a] -> [a] -> [a]
(\\\) [] _ = []
(\\\) l [] = l 
(\\\) l (h:t) = (\\\) (delete h l) t

{- 2. Considere o seguinte tipo de dados para representar uma sequência em que os elementos podem ser
acrescentados à esquerda (Inicio) ou à direita (Fim) da sequência. -}

data Seq a = Nil | Inicio a (Seq a) | Fim (Seq a) a

{- (a) Defina a função primeiro :: Seq a -> a que recebe uma sequência não vazia e devolve o
primeiro elemento. -}

primeiro :: Seq a -> a
primeiro (Inicio n s) = n 
primeiro (Fim Nil n) = n 
primeiro (Fim s n) = primeiro s 

--(b) Defina a função semUltimo :: Seq a -> Seq a que recebe uma sequência não vazia e devolve a sequência sem o seu último elemento.

semUltimo :: Seq a -> Seq a
semUltimo (Inicio n Nil) =  Nil
semUltimo (Inicio n s) =  Inicio n (semUltimo s)
semUltimo (Fim s n) = s

-- 3. Considere o seguinte tipo para representar árvores binárias.

data BTree a = Empty | Node a (BTree a) (BTree a)

{- (a) Defina uma função prune :: Int -> BTree a -> BTree a, que remove de uma árvore todos
os elementos a partir de uma determinada profundidade. -}

prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty 
prune 0 _ = Empty 
prune n (Node r e d) = Node r (prune (n-1) e ) (prune (n-1) d)

{- (b) Defina uma função semMinimo :: (Ord a) => BTree a -> BTree a que remove o menor
elemento de uma árvore binária de procura não vazia. -}

semMinimo :: (Ord a) => BTree a -> BTree a
semMinimo Empty = Empty 
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d 


{- 4. O problema das N rainhas consiste em colocar N rainhas num tabuleiro de xadrez com N linhas e N colunas, de tal forma que
nenhuma rainha está amea¸cada por outra. Note que uma rainha amea¸ca todas as posições que estão na mesma linha, na mesma
coluna ou nas mesmas diagonais. Uma forma de representar estas soluções é usando listas de strings. O exemplo representa uma solução 
para este problema quando N é 4. -}

type Tabuleiro = [String]

{- exemplo :: Tabuleiro
   exemplo = ["..R.",
              "R...",
		              "...R",
		              ".R.."] -}

{- (a) Defina a função posicoes :: Tabuleiro -> [(Int,Int)] que determina as posições (coluna e linha) onde se encontram as rainhas num 
tabuleiro, de tal forma que posicoes exemplo == [(2,0),(0,1),(3,2),(1,3)]. -}

posicoes :: Tabuleiro -> [(Int,Int)]
posicoes l = foldl (\acc y -> acc ++ (foldl (\acc2 x -> if (l !! y) !! x == 'R' then acc2 ++ [(x,y)] else acc2)) [] [0..(length (head l) - 1)]) [] [0..(length l - 1)]

{- Nota:
(!!) :: [a] -> Int -> a
List index (subscript) operator, starting from 0. It is an instance of the more general genericIndex, which takes an index of any integral type.
Let us assume that you need help reading this. The first line tell us that (!!) is a function that takes a list of things ([a]) and an Int then gives you 
back one of the thing in the list (a). The descriptions tells you what it does. It will give you the element of the list indexed by the Int. 
So, xs !! i works like xs[i] would in Java, C or Ruby.-}

{- (b) Usando a função anterior, defina a função valido :: Tabuleiro -> Bool que testa se num tabuleiro nenhuma rainha ataca outra. No caso do 
tabuleiro exemplo a resposta deve ser True. Note que pode testar se duas rainhas estão na mesma diagonal vendo se a soma ou a diferença
entre a linha e a coluna em que estão colocadas são iguais. -}

valido :: Tabuleiro -> Bool
valido l = foldl (\acc (x,y) -> if length (filter (\(a,b) -> (a,b) /= (x,y) && (a == x || b == y || a - b == x - y || b - a == y - x)) (posicoes l)) > 0 then False else acc) True (posicoes l)

{- (c) Utilizando funções de ordem superior, defina a função bemFormado :: Int -> Tabuleiro -> Bool que dado um tamanho n e um tabuleiro t 
testa se este é bem formado, isto é, tem n linhas, n colunas, n rainhas, e os restantes caracteres do tabuleiro são o ’.’ . -}

bemFormado ::  Int -> Tabuleiro -> Bool
bemFormado n tab = length tab == n && foldr (\x -> (&&) $ (==) n $ length x) True tab && foldl (\acc (x,y) -> if (tab !! y) !! x == 'R' then acc + 1 else acc) 0 [(a,b) | a <- [0..n - 1], b <- [0..n - 1]] == n