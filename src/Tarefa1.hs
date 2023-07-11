-- |
-- = Realização da __/Tarefa1/__
module Tarefa1 where

import System.Random
import Data.List 
import Types

{-| Introdução :
Nesta tarefa tinhamos por objetivo gerar o labirinto do pacman. Para tal tinhamos de implementar a função generateMaze.

Objetivos : 
Optamos por criar primeiro o labirinto e só depois implementar as suas restrições (como o túnel e a casa fantasma). Pareceu-nos o mais lógico e o mais simples de
fazer. Escolhemos esta estratégia para minimizar os erros e os "bugs", visto que foi o nosso primeiro grande contacto com um projeto destas dimensões.
 
Conclusão:
A função fez o que era esperado e passou em todos os nossos teste. No entanto, e revisitando a tarefa agora passado quase um semestre, concluimos que há pontos em
que podemos melhorar. Por exemplo, há um exessivo número de "where's" utilizados. Poderiamos também implementar a função ghostpart de uma maneira mais eficiente,
talvez com o recurso a map e/ou filter. 
De qualquer maneira a função funciona como pressuposto e pelo que consideramos que foi concluida com sucesso. 
-}

{- |A Função generateRandoms dada uma semente, retorna uma lista n inteiros gerada aliatoriamente

generateRandoms n seed = let gen = mkStdGen seed ->  cria um gerador aleatorio

in take n $ randomRs (0,99) gen -> pega nos primeiros n elementos numa serie infinita de numeros aleatorios entre 0 e 99
-}
generateRandoms :: Int -> Int -> [Int]
generateRandoms n seed = let gen = mkStdGen seed 
                        in take n $ randomRs (0,99) gen 

-- | Número aliatório gerado com uma seed
nrAleatorio :: Int -> Int
nrAleatorio seed = head $ generateRandoms 1 seed


-- |Converte uma lista numa lista de lista de tamanho n 
subLista :: Int -> [a] -> [[a]]
subLista _ [] = []
subLista n l = take n l: subLista n (drop n l)


{- | Converte um Integer numa Piece
| x == 3 = Food Big -> 3 <=> Food Big
| x >= 0 &&  x < 70 = Food Little -> 0 <= n < 70 <=> Food Little
| otherwise = Wall -> 70 < n <= 99 <=> Wall
-}
convertPiece :: Int -> Piece
convertPiece x 
   | x == 3 = Food Big 
   | x >= 0 &&  x < 70 = Food Little 
   | otherwise = Wall 


-- | Converte um Corridor numa string 
printCorridor :: Corridor -> String
printCorridor [] = "\n"
printCorridor (x:xs) = ( show x ) ++ printCorridor xs 

-- | Converte uma lista de Integers num Corridor
converteCorredor :: [Int] -> Corridor
converteCorredor [] = []
converteCorredor (x:xs) = convertPiece x : converteCorredor xs


-- | Converte uma lista de listas de Integers num Maze 
converteLabirinto :: [[Int]] -> Maze 
converteLabirinto [] = []
converteLabirinto (x:xs) = converteCorredor x : converteLabirinto xs

-- | Formato da casa dos Corridores da casa fantasma. Dependendo do tamanho dos corridores do maze, o tamanho da casa dos fantasmas também varia
-- | No caso de comprimento do labirinto ser par, a entrada da casa fantasma vai ter comprimento 2, de maneira a ficar centralizada
-- | No caso de o comprimento do labirinto ser ímpar, a entrada da casa fantasma vai ter comprimento 3, de maneira a ficar centralizada
ghostHome :: Int -> Maze
ghostHome x 
    | even x = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty], -- No caso de comprimento do labirinto ser par, a entrada da casa fantasma vai ter comprimento 2, de maneira a ficar centralizada
                [Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall,Empty],
                [Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall,Empty],
                [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Empty],
                [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] 
    | otherwise = [[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty], -- No caso de o comprimento do labirinto ser ímpar, a entrada da casa fantasma vai ter comprimento 3, de maneira a ficar centralizada
                   [Empty, Wall, Wall, Wall, Empty, Empty, Empty, Wall, Wall, Wall,Empty],
                   [Empty, Wall, Empty, Empty,Empty, Empty, Empty, Empty, Empty, Wall,Empty],
                   [Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Wall,Wall,Empty], 
                   [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]] 

-- | A função walls cria um Corridor cheio de Paredes (para as paredes inferior de superiores)  
walls :: Int -> Corridor 
walls x = replicate x Wall

-- | A função walls2 adiciona paredes laterais à volta do Maze 
walls2 ::  Maze -> Maze 
walls2  [] = []
walls2 (x:xs) = concat[[Wall],x,[Wall]] : walls2  xs

-- | Junta as paredes superiores e inferiores com o restante do maze  
-- | limMaze x y z = (walls x) : (walls2  (createMiddle (x-2) (y-2) z)) ++ [(walls x)] -> Gerámos (x-2) e (y-2) devido à colocação das paredes no fim e início dos Corridores (excepto o primeiro e último)
limMaze :: Int -> Int -> Int -> Maze 
limMaze x y z = (walls x) : (walls2  (createMiddle (x-2) (y-2) z)) ++ [(walls x)] 

-- | Gera aliatoriamente as Pieces que constituem os restantes Corridores
 
createMiddle :: Int -> Int -> Int -> Maze
createMiddle x y z = converteLabirinto $ subLista x (generateRandoms (x*y) z)

{- |

A Função unir une a 3 partes de um labirinto:

A de cima 

A do meio (contituida pelos túneis e pela casa fantasma)

A de baixo (obtida através de tirarmos as outras duas de um labirnto)

Quando o y é par retiramos div y 2 - 2 pois a casa fantasma tem de estar ​ X-1 peças de distância da parede superior 

Sendo o y ímpar, o labirinto vai ter o mesmo número de corridores para cima e para baixo do corridor do Túnel (nx+5 pois o labirinto vai ter 5 corridores)

O "nx       = div y 2 - 2"  é numero de corredores da parte de cima que não são "influenciados" pelo labirinto
-}
unir x y z | even y       = take (nx-1) mazeEven  ++  ghostP ++ drop (nx+4) (mazeEven) 
           | odd y        = take nx mazeOdd       ++  ghostP ++ drop (nx+5) (mazeOdd) 
           where mazeEven = replaceIncorridorEven (limMaze x y z ) (y `div` 2 - 1) Empty
                 mazeOdd  = replaceIncorridorOdd (limMaze x y z ) (y `div` 2) Empty
                 ghostP   = ghostpart x y z
                 nx       = div y 2 - 2 

-- | Trata de juntar o que vem antes e depois com a casa dos fantasmas nos seus varios corredores
ghostpart :: Int -> Int -> Int -> Maze
ghostpart x y z | even x =  [take nxh hmazeE ++ (head ghostH)      ++ drop nxt hmazeE]  ++  
                            [take nxh t2mazeE ++ (taken1 ghostH 2) ++ drop nxt t2mazeE] ++ 
                            [take nxh t3mazeE ++ (taken1 ghostH 3) ++ drop nxt t3mazeE] ++ 
                            [take nxh t4mazeE ++ (taken1 ghostH 4) ++ drop nxt t4mazeE] ++ 
                            [take nxh t5mazeE ++ (taken1 ghostH 5) ++ drop nxt t5mazeE] 
                | odd x =   [take nxho hmaze0  ++ (head ghostH )    ++ drop nxto hmaze0 ] ++
                            [take nxho t2maze0 ++ (taken1 ghostH 2) ++ drop nxto t2maze0] ++
                            [take nxho t3maze0 ++ (taken1 ghostH 3) ++ drop nxto t3maze0] ++
                            [take nxho t4maze0 ++ (taken1 ghostH 4) ++ drop nxto t4maze0] ++
                            [take nxho t5maze0 ++ (taken1 ghostH 5) ++ drop nxto t5maze0]
                        where mazeEven = mazetocorridor x y z            
                              nxh      = (div x 2) - 5
                              nxt      = (div x 2) + 5
                              ghostH   = ghostHome x 
                              mazeOdd  = mazetocorridor x y z
                              nxho     = (div x 2) - 5 
                              nxto     = (div x 2) + 6
                              hmazeE   = head mazeEven
                              t2mazeE  = taken1 mazeEven 2
                              t3mazeE  = taken1 mazeEven 3
                              t4mazeE  = taken1 mazeEven 4
                              t5mazeE  = taken1 mazeEven 5
                              hmaze0   = head mazeOdd
                              t2maze0  = taken1 mazeOdd 2
                              t3maze0  = taken1 mazeOdd 3
                              t4maze0  = taken1 mazeOdd 4
                              t5maze0  = taken1 mazeOdd 5

                  

{- | Função auxiliar. Utilizamos esta função para 
taken1 [[]] _ = []  Base da recursão. No caso do n > numero de Corridores do maze, então vai dar um Corridor vazio 
taken1 (x:xs) n | n == 1 = x  No caso de queremos o 1º Corridor
                | n /= 1 = taken1 xs (n-1) eliminamos o 1º Corridor porque não era esse que procuravamos, chamando a função recursivamente                
-}
taken1 :: [[a]] -> Int -> [a]
taken1 [[]] _ = [] 
taken1 (x:xs) n | n == 1 = x 
                | n /= 1 = taken1 xs (n-1) 

-- | Função auxiliar. Retira os Corridores que vão ser afetados pela inserção da casa dos fantasmas no maze
mazetocorridor :: Int -> Int -> Int -> Maze
mazetocorridor x y z | even y = [ taken1 mazeEven (div y 2 - 2)  ] ++ 
                                [ taken1 mazeEven (div y 2 - 1)  ] ++ 
                                [ taken1 mazeEven (div y 2)      ] ++
                                [ taken1 mazeEven (div y 2 + 1)  ] ++
                                [ taken1 mazeEven (div y 2 + 2)  ] 
                     | odd y =  [ taken1 mazeOdd (div y 2 - 1)     ] ++  
                                [ taken1 mazeOdd (div y 2)         ] ++
                                [ taken1 mazeOdd (div y 2 + 1)     ] ++
                                [ taken1 mazeOdd (div y 2 + 2)     ] ++
                                [ taken1 mazeOdd (div y 2 + 3)     ]
                     where mazeEven = replaceIncorridorEven (limMaze x y z) (y `div` 2 - 1) Empty                                 
                           mazeOdd  = replaceIncorridorOdd (limMaze x y z) (y `div` 2 ) Empty
                                


            
-- | Substitui uma lista de indice 0 por uma peça                 
replaceInList :: Int -> Piece -> Corridor -> Corridor
replaceInList n p [] =[]
replaceInList 0 p (x:xs) = p:xs 
replaceInList n p (x:xs) = x:replaceInList (n-1) p xs
 
-- | Substitui uma peça num Labirinto para meter os tuneis (Impares)
-- | Num Maze cujo Y seja ímpar apenas vamos inserir um túnel (exatamente a metade)
replaceIncorridorOdd :: Maze -> Int -> Piece -> Maze
replaceIncorridorOdd [] y t = []
replaceIncorridorOdd (x:xs) 0 t = replaceInList (length x - 1) t (replaceInList 0 t x) : xs   
replaceIncorridorOdd (x:xs) y t = x : replaceIncorridorOdd xs (y-1) t

-- |  Substitui uma peça num Labirinto para meter os tuneis (Pares)
replaceIncorridorEven :: Maze -> Int -> Piece -> Maze
replaceIncorridorEven [] y t = []
replaceIncorridorEven (x1:x2:ts) 0 t = replaceInList (length x1 - 1) t (replaceInList 0 t x1) : replaceInList (length x2 -1) t (replaceInList 0 t x2) : ts  --Num Maze cujo Y seja par, vamos inserir 2 túneis. Deste modo ficam os mesmo número de Corridores acima e abaixo do Corridores dos túneis. 
replaceIncorridorEven (x:xs) y t = x : replaceIncorridorEven xs (y-1) t


-- | Gera o maze 
-- | O labirinto deve conter no minimo 15 de comprimento e 10 de altura ec hamamos a função unir que nos devolve um Labirinto (Maze) que cumpre os vários requisitos 

generateMaze :: Int -- ^ largura
    -> Int  --- ^ altura
    -> Int -- ^ seed
    -> Maze -- ^ O Maze Final
generateMaze x y z | x<15 || y<10 = error "Não cumpre as medidas minimas"  
                   | otherwise = unir x y z 

-- | Função auxiliar. Permite um melhor visionamento da função generateMaze com o objetivo de detetar potenciais erros
imprimeLabirinto :: Int -> Int -> Int -> IO ()
imprimeLabirinto x y z = do putStrLn (printMaze (generateMaze x y z)) 

 
{- |

__Dicionário de variáveis__ (Para efeitos ilustrativos, nesta parte são melhor definidas as variáveis mais utilizadas ao longo da Tarefa1) :

mazeEven -> Labirinto cujo y é ímpar, ou seja, o tunel vai ter 1 de altura

mazeOdd  -> Labirinto cujo y é par  , ou seja, o tunel vai ter 2 de altura

ghostH   -> A casa fantasma (que varia de comprimento se o x é par ou ímpar)

nxh      -> Número de peças no inicio do corridor não afetadas pela introdução da casa fantasma (no caso de x par)

nxt      -> Número de peças no fim do corridor não afetadas pela introdução da casa fantasma (no caso de x par)

nxho     -> Número de peças no inicio do corridor não afetadas pela introdução da casa fantasma (no caso de x ímpar)

nxto     -> Número de peças no fim do corridor não afetadas pela introdução da casa fantasma (no caso de x ímpar) 

ghostP   -> Lista dos vários corredores onde a GhostHome (casa dos fantasmas) está inserida

t2mazeE  -> Tira o Corridor 2 do labirinto resultante da função mazetocorridor de um labirinto par  

t3mazeE  -> Tira o Corridor 3 do labirinto resultante da função mazetocorridor de um labirinto par

t4mazeE  -> Tira o Corridor 4 do labirinto resultante da função mazetocorridor de um labirinto par

t5mazeE  -> Tira o Corridor 5 do labirinto resultante da função mazetocorridor de um labirinto par

t2maze0  -> Tira o Corridor 2 do labirinto resultante da função mazetocorridor de um labirinto ìmpar

t3maze0  -> Tira o Corridor 3 do labirinto resultante da função mazetocorridor de um labirinto ìmpar

t4maze0  -> Tira o Corridor 4 do labirinto resultante da função mazetocorridor de um labirinto ìmpar

t5maze0  -> Tira o Corridor 5 do labirinto resultante da função mazetocorridor de um labirinto ìmpar


Notas adicionais : 

Ao longo deste trabalho utiliza-se a conotoção Maze para os Labirintos e Corridores para os vários corredores de cada labirinto 
-}


-- | __Testes__ locais (servem para determinar se a função funciona ou não)

-- | Teste de comprimento e altura. Se der True então o comprimento e a altura estão certas
testaGeraLabirinto :: Int -> Int -> Int -> Bool
testaGeraLabirinto x y z | altura x y z && comprimento (generateMaze x y z) = True
                         | otherwise = False
-- | Testa se a altura está correta
altura :: Int -> Int -> Int -> Bool
altura x y z = if y == length (generateMaze x y z) then True else False -- A altura só está correta se y for igual ao numero de corredores

-- | Compara o comprimento de cada corridor. Caso algum seja diferente do anterior dá falso
comprimento :: Maze -> Bool
comprimento [x,y] = if length x == length y then True else False
comprimento mz = if length (head mz) == length (taken1 (scnd mz) 1) then comprimento (tail mz) else False

-- | Função auxiliar. Utilizada para dar os segundos corredores dos labirintos
scnd :: Maze -> Maze
scnd [[]] = [[]]
scnd [x] = [[]]
scnd (x:y:z) = y : scnd z