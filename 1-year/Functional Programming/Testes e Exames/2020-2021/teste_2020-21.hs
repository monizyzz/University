import Data.Char
import Data.List
import Data.Maybe

-- Teste 2020/2021

{- 1. Apresente uma definição recursiva da função (\\\\) :: Eq a => [a] -> [a] -> [a] 
que retorna a lista resultante de remover (as primeiras ocorrências) dos elementos da segunda lista da primeira.
Por exemplo, (\\) [1,2,3,4,5,1,2] [2,3,4,1,2] == [5,1]. -}

(\\\\) :: Eq a => [a] -> [a] -> [a]
(\\\\) [] _ = []
(\\\\) l [] = l
(\\\\) l (h:t) = (\\\\) (delete h l) t
    where
        delete :: Eq a => a -> [a] -> [a]
        delete _ [] = []
        delete x (h:t)
            | x == h = t
            | otherwise = h : delete x t

{- 2. Considere o tipo MSet a para representar multi-conjuntos de elementos de a: type MSet a = [(a,Int)].
Considere ainda que nestas listas não há pares cuja primeira componente coincida,
nem cuja segunda componente seja menor ou igual a zero -}

type MSet a = [(a,Int)]

{- (a) Defina a função removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)] que remove um elemento a um multi-conjunto.
Se o elemento não existir, deve ser retornado o multi-conjunto recebido.
Por exemplo, removeMSet 'c' [('b',2), ('a',4), ('c',1)] == [('b',2),('a',4)]. -}

removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet _ [] = []
removeMSet x ((a,b):t) | x == a = if b == 1 then t else (a,b-1):t
                       | otherwise = (a,b) : removeMSet x t

{- (b) Usando uma função de ordem superior, defina a função calcula :: MSet a -> ([a],Int) que,
numa única travessia do multi-conjunto, calcula simulanemente a lista (sem repetidos) de elementos do multi-conjunto e o número total de elementos.
Por exemplo, calcula [('b',2), ('a',4), ('c',1)] == (['b','a','c'], 7). -}

calcula :: MSet a -> ([a],Int)
calcula = foldr (\(a,n) (elems, total) -> (a : elems, n + total)) ([],0)


{- 3. Defina a função partes :: String -> Char -> [String] que parte uma string pelos pontos onde um dado caracter ocorre.
Por exemplo, partes "um;bom;exemplo;" ';' == ["um","bom","exemplo"] e partes "um;exemplo;qualquer" ';' == ["um","exemplo",'"qualquer"]. -}

partes :: String -> Char -> [String]
partes [] _ = []
partes (h:t) c | h == c = partes t c
               | otherwise = (h : parte) : partes resto c
            where
              (parte, resto) = break (== c) t

-- 4. Considere o seguinte tipo para representar árvores binárias de procura. 

data BTree a = Empty | Node a (BTree a) (BTree a)

a1 = Node 5 (Node 3 Empty Empty)
            (Node 7 Empty (Node 9 Empty Empty))

-- (a) Defina a função remove :: Ord a => a -> BTree a -> BTree a que remove um elemento de uma árvore binária de procura.

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node a l r) | x > a = Node a l (remove x r)
                      | x < a = Node a (remove x l) r
                      | otherwise = case (l,r) of 
                            (Empty, r) -> r
                            (l, Empty) -> l
                            (l, r) -> let (min, sMin) = minSMin r         in Node min l sMin

minSMin :: BTree a -> (a, BTree a)
minSMin (Node e Empty r) = (e, r)
minSMin (Node e l r) = let (min, sMin) = minSMin l in (min, Node e sMin r)

{- (b) Defina BTree a como uma instância da classe Show de forma a que show a1 produza a string ((* <-3-> *) <-5-> (* <-7-> (* <-9-> *))) -}

instance Show a => Show (BTree a) where
    show :: Show a => BTree a -> String
    show Empty = "*"
    show (Node e l r) = "(" ++ show l ++ " <-" ++ show e ++ "-> " ++ show r ++ ")"

{- 5. Apresente uma definição da função sortOn :: Ord b => (a -> b) -> [a] -> [a] que ordena uma lista comparando os resultados 
de aplicar uma função de extração de uma chave a cada elemento de uma lista.
Por exemplo, sortOn snd [(3,1),(2,5),(1,2)] == [(3,1),(1,2),(2,5)]. -}

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []

-- 6. Considere o seguinte tipo para representar um sistema hierárquico de ficheiros.  

data FileSystem = File Nome | Dir Nome [FileSystem]
type Nome = String

fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]],
                 Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"] ]

{- (a) Defina a função fichs :: FileSystem -> [Nome] que lista o nome de todos os ficheiros de um file system. -}

fichs :: FileSystem -> [Nome]
fichs (File nome) = [nome]
fichs (Dir _ files) = concatMap fichs files

{- (b) Defina a função dirFiles :: FileSystem -> [Nome] -> Maybe [Nome] 
que lista o nome dos ficheiros de um file system que estão numa determinada path. 
Se a path não for válida, a função deve devolver Nothing. 
Por exemplo, dirFiles fs1 ["usr","xxx"] == Just ["abc.txt","readme"]. -}

dirFiles :: FileSystem -> [Nome] -> Maybe [Nome]
dirFiles (File name) [] = Just [name]
dirFiles (Dir name files) (h:t)
    | h == name = 
        let results = mapMaybe (`dirFiles` t) files in
            if null results then
                Nothing
            else
                Just $ concat results
    | otherwise = Nothing
dirFiles _ _ = Nothing

{- (c) Defina a função listaFich :: FileSystem -> IO () que lê uma path do teclado e imprime no ecrã
os nomes dos ficheiros que estão na diretoria indicada pela path. 
A path deve ser lida como uma string com o formato usual (por exemplo: "usr/xxx/PF"). 
Se a path não for válida, deve ser escrita a mensagem "Não é uma diretoria." -}

listaFich :: FileSystem -> IO ()
listaFich fs = do
    putStr "> "
    path <- getLine
    case dirFiles fs (partes path '/') of 
        Just files -> print files
        Nothing -> putStrLn "Não é uma diretoria."