-- Exame 2017/2018

{- 1. Apresente uma definição recursiva da função (pré-definida) (!!) :: [a] -> Int -> a que dada uma lista e um inteiro, calcula o elemento da 
lista que se encontra nessa posição (assume-se que o primeiro elemento se encontra na posição 0). Por exemplo, (!!) [10,20,30] 1 corresponde a 20.
Ignore os casos em que a função não se encontra definida (i.e., em que a posição fornecida não corresponde a nenhuma posição válida da lista). -}

(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h 
(!!!) (h:t) n = (!!!) t (n-1)

--2. Considere o seguinte tipo para representar movimentos de um robot.

data Movimento = Norte | Sul | Este | Oeste deriving Show

{- Defina a função posicao :: (Int,Int) -> [Movimento] -> (Int,Int) que, dada uma posição inicial (abcissa e ordenada) e uma lista de movimentos 
(um movimento para Norte aumenta a ordenada e para Este aumenta a abcissa), calcula a posição final do robot depois de efectuar essa
sequência de movimentos. -}

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao i [] = i 
posicao (x,y) (m:ms) = posicao ( case m of Norte -> (x,y+1)
                                           Sul -> (x,y-1)
                                           Este -> (x+1,y)
                                           Oeste -> (x-1,y)) ms

{- 3. Apresente uma definição recursiva da função any :: (a -> Bool) -> [a] -> Bool que testa se um predicado é verdade para algum elemento 
de uma lista. Por exemplo, any odd [1..10] == True. -}

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False 
any' f (h:t) = f h || any' f t 

{- 4. Considere o sequinte tipo type Mat a = [[a]] para representar matrizes. Defina a função triSup :: Num a => Mat a -> Bool que testa se uma
matriz quadrada é triangular superior (i.e., todos  os elementos abaixo da diagonal são nulos). Esta função deve devolver True para a matriz
[[1,2,3], [0,4,5], [0,0,6]]. -}

type Mat a = [[a]]

triSup :: (Num a, Eq a) => Mat a -> Bool
triSup [] = True 
triSup (h:t) = let l = map head t
                   rm = map tail t
               in all (==0) l && triSup rm

{- 5. Defina um programa movimenta :: IO (Int,Int) que lê uma sequência de comandos do teclado (’N’ para Norte, ’S’ para Sul, ’E’ para Este, ’O’ 
para Oeste e qualquer outro caracter para parar) e devolve a posição final do robot (assumindo que a posição inicial é (0,0)). -}

movimenta :: IO (Int,Int)
movimenta = moveFrom (0,0)

moveFrom :: (Int,Int) -> IO (Int,Int)
moveFrom (x,y) = do
    dir <- getChar
    case dir of 'n' -> moveFrom (x,y+1)
                's' -> moveFrom (x,y-1)
                'e' -> moveFrom (x+1,y)
                'o' -> moveFrom (x-1,y)
                otherwise -> return (x,y)

{- 6. Considere o tipo Imagem para representar imagens compostas por quadrados (apenas com coordenadas positivas).
Ao lado apresenta-se um exemplo de uma destas imagens constituída por três quadrados (cujos
lados têm dimensão 5, 4 e 2). -}

data Imagem = Quadrado Int | Mover (Int,Int) Imagem | Juntar [Imagem]

{- ex :: Imagem
   ex = Mover (5,5) (Juntar [Mover (0,1) (Quadrado 5), Quadrado 4, Mover (4,3) (Quadrado 2)])
(a) Defina a função vazia :: Imagem -> Bool que testa se uma imagem não tem nenhum quadrado. A função devolve False para o exemplo acima. -}

vazia :: Imagem -> Bool
vazia (Quadrado c) = False 
vazia (Mover (x,y) t) = vazia t
vazia (Juntar []) = True 
vazia (Juntar l) = all vazia l 

{- (b) Defina a função maior :: Imagem -> Maybe Int que calcula a largura do maior quadrado de uma imagem. No exemplo acima, maior ex == Just 5. 
Note que a imagem pode não ter quadrados. -}

maior :: Imagem -> Maybe Int
maior l | vazia l = Nothing
        | otherwise = Just (maximum (quads l))
                      where quads :: Imagem -> [Int]
                            quads (Quadrado x) = [x]
                            quads (Mover _ l) = quads l
                            quads (Juntar l) = concat (map quads l)

{- (c) Defina Imagem como uma instância de Eq de forma a que duas imagens são iguais sse forem compostas pelos mesmos quadrados nas mesmas posições.
Por exemplo, a imagem ex acima deverá ser igual a Juntar [Mover (5,5) (Quadrado 4), Mover (5,6) (Quadrado 5), Mover (9,8) (Quadrado 2)]. -}

instance Eq Imagem where
    x == y = let a = posicoesI (0,0) x
                 b = posicoesI (0,0) y
             in (all (pertence a) b)

posicoesI :: (Int,Int) -> Imagem -> [Imagem]
posicoesI (x,y) (Quadrado a) = [(Mover (x,y) (Quadrado x))]
posicoesI (x,y) (Mover (a,b) l) = posicoesI (x+a,y+b) l
posicoesI (x,y) (Juntar l) = concat (map (posicoesI (x,y)) l)

pertence :: [Imagem] -> Imagem -> Bool
pertence [] _ = False
pertence ((Mover (x1,y1) (Quadrado d1)):t) (Mover (x2,y2) (Quadrado d2)) | x1==x2 && y1==y2 = d1 == d2
                                                                         | otherwise = pertence t (Mover (x2,y2) (Quadrado d2))