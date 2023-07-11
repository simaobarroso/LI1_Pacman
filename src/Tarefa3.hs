-- |
-- = Realização da __/Tarefa3/__

module Tarefa3 where

import Types

{- |
A Tarefa 3 tem como objetivo optimizar a leitura de um maze.

Nós conseguimos alcançar o propósito desta tarefa, pegando num corridor e transformá-lo numa instructions. De seguida usamos essa função no maze inteiro e depois vimos se havia peças repetidas dentro de cada instruct, para organizar o labirinto melhor.
-}

-- | Pega num corridor e transforma numa instruction
corridorinstruction :: Corridor -> Instruction
corridorinstruction l = (Instruct (compressCorridor l))
                    where compressCorridor [] = []
                          compressCorridor (x:xs) = ((length (takeWhile (==x) (x:xs)),x)) : (compressCorridor (dropWhile (==x) (xs)))



-- | A função 'mazeToInst' converte um __Maze__ numa  __Instruction__ , usando a corridorinstructionem todos os corridors do Maze.
mazeToInst :: Maze -> Instructions
mazeToInst [] = []
mazeToInst (x:xs) = ((corridorinstruction x ) : (mazeToInst xs))
                        
-- |    A função 'indexOf' dá o indice de um elemento de uma lista.
indexOf :: Instruction -- ^ Elemento para procurar
   -> [(Int, Instruction)] -- ^ Lista onde se vai procurar esse elemento
   -> Int -- ^  Dá o indice do elemento ou se nao houver nenhum igual, dá um um numero negativo 
indexOf _ [] = -1
indexOf l ((i,x):xs) | l == x = i
            | otherwise = indexOf l xs

-- | A função 'findRepeats' substitui os os elementos iguais por Repeats
findRepeats :: [(Int, Instruction)] -> [(Int, Instruction)] -- ^ Pedimos duas Listas iguais para as trabalhar com ambas 
                -> [(Int, Instruction)] -- ^ Dá a Lista final que queremos, mas com o indice atras
findRepeats [] _ = []
findRepeats ((i,x):xs) tudo | (indexOf x tudo) == i = (i,x):(findRepeats xs tudo)
              | otherwise = (i,(Repeat (indexOf x tudo))) : (findRepeats xs tudo)
              
-- | A função 'cutIndex' corta o indice que está nas nossas lista, para obtermos a lista pretendida
cutIndex :: [(Int, Instruction)] -- ^ A lista dividida em (indice, Instruction)
    -> Instructions -- ^ O indice é retirado
cutIndex [] = []
cutIndex ((n,x):xs) = x : cutIndex xs

-- | A função 'compactMaze' é a função final que junta tudo.
compactMaze :: Maze -> Instructions
compactMaze [] = []
compactMaze l = cutIndex(findRepeats (zip [0..] (mazeToInst l)) (zip [0..] (mazeToInst l)))
