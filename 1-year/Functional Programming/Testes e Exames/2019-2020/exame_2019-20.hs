import Data.Char
import Data.List
import Data.Maybe

-- Exame 2019/2020

-- 1. Apresente uma definição recursiva das seguintes funções (pré-definidas) sobre listas:

-- (a) inits:: [a] -> [[a]] que calcula a lista dos prefixos de uma lista. Por exemplo, inits [11,21,13] corresponde a [[],[11],[11,21],[11,21,13]].

inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

-- (b) isPrefix0f:: Eq a => [a] -> [a] -> Bool que testa se uma lista é prefixo de outra. Por exemplo, isPrefix0f [10,20] [10,20,30] corresponde a True enquanto que isPrefix0f [10,30] [10,20,30] corresponde a False.

isPrefix0f:: Eq a => [a] -> [a] -> Bool
isPrefix0f [] l = True
isPrefix0f l [] = False 
isPrefix0f (x:xs) (y:ys) | x == y = isPrefix0f xs ys 
                         | otherwise = False 

-- 2. Considere o seguinte tipo para representar árvores binárias. 

data BTree a = Empty
             | Node a (BTree a) (BTree a)
    deriving Show

{- (a) Defina a função folhas :: BTree a -> Int que calcula o número de folhas (i.e., nodos sem descendentes) da árvore. -}

folhas :: BTree a -> Int
folhas  Empty = 0
folhas (Node r Empty Empty) = 1 
folhas (Node r e d) = folhas e + folhas d

{- (b) Defina a função path :: [Bool] -> BTree a -> [a] que, dado um caminho (False corresponde a esquerda e True a direita) e uma árvore, dá a lista com a informação dos nodos por onde esse caminho passa. -}

path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node r _ _) = [r]
path (h:t) (Node r e d) = r : path t (if h then d else e)

{- 3. Uma representação possível de polimómios é pela sequência dos coeficientes - têm que se armazenar também os coeficientes nulos pois será a posição do coeficiente na lista que dará o grau do monómio. -}

type Polinomio = [Coeficiente]
type Coeficiente = Float

{- A representação do polinómio 2x^5 - 5x^3 será então [0,0,0,-5,0,2], que corresponde ao polinómio 0x^0 + 0x^1 + 0x^2 - 5x^3 + 0x^4 + 2x^5. Nas questões que se seguem, use, sempre que possível, funções de ordem superior. -}

{- (a) Defina a operação valor :: Polinomio -> Float -> Float que calcula o valor do polinómio para um dado x. -}

valor :: Polinomio -> Float -> Float
valor p x = foldr (\(g,c) acc -> acc + c * x ^ g) 0 $ zip [0..] p

{- (b) Defina a operação deriv :: Polinomio -> Polinomio que calcula a derivada de um polinómio. -}

deriv :: Polinomio -> Polinomio
deriv = map (uncurry (*)) . tail . zip [0..]

{- (c) Defina a operação soma :: Polinomio -> Polinomio -> Polinomio de adição de polinómios. -}

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = zipWith (+) (pad p1) (pad p2)
    where max_len = max (length p1) (length p2)
          pad p = p ++ replicate (max_len - length p) 0

{- 4. Considere a seguinte definição para representar matrizes:
type Mat a = [[a]]. ex = [[1,4,3,2,5], [6,7,8,9,0], [3,5,4,9,1]] representa a matriz abaixo desenhada. 

| 1 4 3 2 5 |
| 6 7 8 9 0 |
| 3 5 4 9 1 |

-}
type Mat a = [[a]]

{- (a) Defina a função quebraLinha :: [Int] -> [a] -> [[a]] que recebe uma lista de inteiros s e uma linha l, e produz a lista de segmentos contíguos de l de comprimento indicado em s. 
Por exemplo, quebraLinha [2,3] [1,4,3,2,5] == [[1,4],[3,2,5]]. -}

quebraLinha :: [Int] -> [a] -> [[a]] 
quebraLinha [] _ = []
quebraLinha (x:xs) l = take x l : quebraLinha xs (drop x l)

{- (b) Defina a função fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a] que recebe duas lista de inteiros (com a partição das linhas e das colunas) e uma matriz, e produz a lista de (sub)-matrizes de acordo com essa partição. Por exemplo,fragmenta [2,1] [2,3] ex == [ [[1,4],[6,7]], [[3,2,5],[8,9,0]], [[3,5]], [[4,9,1]] ]. -}

fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]
fragmenta [] _ _ = []
fragmenta (x:xs) c m = fragmentaCols c (take x m) ++ fragmenta xs c (drop x m)

fragmentaCols :: [Int] -> Mat a -> [Mat a]
fragmentaCols [] _ = []
fragmentaCols (x:xs) m = map (take x) m : fragmentaCols xs (map (drop x) m)

{- (c) Defina a função geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int) tal que geraMat (x,y) (a,b) gera aleatoriamente uma matriz com x linhas e y colunas, cujos valores estão compreendidos entre a e b. Sugestão: use a função randomRIO :: Random a => (a,a) -> IO a. -}

{-

geraMat :: (Int,Int) -> (Int,Int) -> IO (Mat Int)
geraMat (x,y) (a,b) = 
    sequence 
    $ replicate y 
    $ sequence 
    $ replicate x 
    $ randomRIO (a,b)

-}