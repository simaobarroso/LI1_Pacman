-- |
-- = Realização da __/Tarefa5/__

module Tarefa5 where 

import Types
import Tarefa1
import Tarefa2
import FileUtils
import System.IO.Unsafe 

{- |
Tanto como a Tarefa 5 e a Tarefa 6 foram realizadas de formas similares 

Ambas as tarefas têm como objetivo controlar automaticamente jogadores. Sendo na tarefa 5 os fantasmas e na tarefa 6 o pacman. Ambos, dependendo do seu modo, têm duas maneiras de reagir. Uma delas onde estão a fugir (denominadas de scatterMode para os fantasmas e normalscatter para o pacman) e outra onde estão a perseguir (denominadas de chaseMode para os fantasmas e megaChase para o pacman). 

Na Tarefa 5, a tarefa cujo objetivo é controlar os fantasmas automaticamente. A nossa função final, a ghostPlay, dá-nos uma lista de plays, sendo uma play por fantasma. Estas plays são nos dadas pelas funções mencionadas anteriormente, a scatterMode e a chaseMode. 

A scatterMode e a chaseMode funcionam de maneiras similares. Ambas têm mais ou menos mesma estrategias. Vêm se têm paredes à sua volta e adaptam-se dependendo de onde o pacman está. Por exemplo, primeiro vêm se têm paredes à sua esquerda, direita e frente, e vêm qual é a coordenada mais perto / mais longe (dependendo se é scatterMode ou chaseMode) do pacman e direcionam-se para lá. 

Estes cálculos são feitos pelas funções calcBetterOrientation's calcBetterOrireverse's e na Tarefa 6 calcBetterOriPac e calcShortOriPac 

Estas funções são seguidas por R, L, F e ou B 

R- (right), vê à sua direita 

L- (left), vê à sua esquerda 

F- (front), vê à sua frente 

B- (back), vê as suas costas 

  

A junção destas letras irá ser para comparar as situações 

por exemplo: 

calcBetterOriFL vai escolher a melhor orientação para ir entre F e L, ou seja, vai ver se a melhor opcao é ir à sua esquerda ou em frente. 

Conclusão: 
A partir deste método conseguimos ter fantasmas a perseguir e a fugir eficientemente. Ao perseguir, vimos que ao adicionar múltiplos fantasmas, se todos nascessem dentro da casa dos fantasmas, vimos que não ter um fantasma ou 8, seria a mesma coisa, por isso, chegamos à conclusão que, dependendo do seu indice, o fantasma iria perseguir para a direita do pacman ou para a esquerda do pacman, assim, caso estejam mais fantasmas no labirinto, será mais fácil perseguir o pacman e encurralá-lo. 
-}

-- | Função final. Um Fantasma pode estar no state Alive ou Dead e muda o seu comportamento dependendo disso. Os primeiros movimentos irão ser da perfil fantasmas
ghostPlay :: State -> [Play]
ghostPlay (State maze [] level) = []
ghostPlay s@(State maze [pacman] level) = []
ghostPlay (State maze players level )
            | isGhost x && deadOrAlive x == Alive = (chaseMode (State maze players level ) (getPlayerID x)) : ghostPlay (State maze xs level )
            | isGhost x && deadOrAlive x == Dead = let xq = (scatterMode (State maze players level ) (getPlayerID x)) in (xq)  : ghostPlay (State maze xs level )
            | otherwise = ghostPlay (State maze (xs++[x]) level)
           where (x:xs) = players

-- | vê se é um fantasma ou não
isGhost :: Player -> Bool
isGhost (Pacman (PacState (x,y,z,t,h,l) q c d )) = False
isGhost (Ghost (GhoState (x,y,z,t,h,l) q )) = True

-- | vê se o fantasma está no modo dead ou Alive
deadOrAlive :: Player -> GhostMode
deadOrAlive (Ghost (GhoState a b )) = b 

-- | vê quantas vidas um player tem
getPlayerLives' :: Player -> Int
getPlayerLives' (Ghost (GhoState (x,y,z,t,h,l) b )) = l

-- | Função que vê as coordenadas de uma certa direção
somacoords :: Orientation -> Coords -> Coords
somacoords o (x,y)
        | o == U = (x-1,y)
        | o == D = (x+1,y)
        | o == L = (x,y-1)
        | o == R = (x,y+1)

-- | numa certa orientação, se for 1 há uma wall ali, se for 0, não há uma wall lá.
verificarwalls :: Orientation -> Maze -> Coords -> Int
verificarwalls o maze (x,y) | (y == 0) || (y == (length (head maze) -1)) =0 
                            | ((maze!!w)!!z) == Wall = 1
                            | otherwise = 0 
            where (w,z) = (somacoords o (x,y))

-- | inverte a orientação
revertorientation :: Orientation -> Orientation
revertorientation x
    | x == R = L
    | x == L = R
    | x == U = D
    | x == D = U

-- | esta função tem a direita consuante a sua orientação, por exemplo, quando um fantasma tem orientação Up , a sua direita seria Right
rightorientation :: Orientation -> Orientation
rightorientation x
    | x == R = D
    | x == L = U
    | x == U = R
    | x == D = L 

-- | Esta função mostra as coordenadas do lado direito (em relação ao fantasma)
rightcoords :: Orientation -> Coords -> Coords 
rightcoords o (x,y) 
    | o == R = (x+1, y)
    | o == L = (x-1, y)
    | o == U = (x, y+1)
    | o == D = (x, y-1) 


-- | esta função faz o mesmo que a função rightorientation só que para a esquerda
leftorientation :: Orientation -> Orientation
leftorientation x        
    | x == R = U
    | x == L = D
    | x == U = L
    | x == D = R

-- | Faz o mesmo que a função rightcoords mas para a esquerda
leftcoords :: Orientation -> Coords -> Coords
leftcoords o (x,y) 
    | o == R = (x-1, y)
    | o == L = (x+1, y)
    | o == U = (x, y-1)
    | o == D = (x, y+1)

-- | Faz o mesmo que a função rightcoords, mas para à sua frente
frontcoords :: Orientation -> Coords -> Coords
frontcoords o (x,y)
    | o == R = (x, y+1)
    | o == L = (x, y-1)
    | o == U = (x-1, y)
    | o == D = (x+1, y)

-- | Dado uma Lista de Players e num Id, dá-nos o respetivo player associado ao id dessa lista
getPlayerById :: [Player] -> Int -> Player
getPlayerById (x:xs) id 
   | (getPlayerID x) == id = x
   | otherwise = getPlayerById xs id

getPacman :: [Player] -> [Player]
getPacman [] = []
getPacman (x:xs)
   | (verificapacman x) = [x]
   | otherwise = getPacman xs

distancia (x, y) (pac, man) = sqrt (((fromIntegral pac -fromIntegral x)^2) + ((fromIntegral man- fromIntegral y)^2))

geraOpcoes :: Coords -> Orientation -> [Coords]
geraOpcoes c ori = [c, (leftcoords ori c)]

          
-- | Número de perfis
nPerfis = 4

-- | Pega na orientação de um jogador, se a orientação for Null, passa a ser U
getPlayerOri :: Player -> Orientation
getPlayerOri player | ori == Null = U
                    | otherwise = ori
     where ori =  getPlayerOrientation player

-- | calcula as distância entre os pontos onde o fantasma pode ir e para onde quer ir e decide para onde vai
-- | Neste caso, o fantasma poderia ir à sua direita ou esquerda
calcBetterOrientationRL :: Player -> Player -> Orientation
calcBetterOrientationRL gho pacman
    | disL < disR = leftorientation ori
    | otherwise = rightorientation ori
    where casas = ( mod (getPlayerID gho) nPerfis)
          pc = ncasas casas (getPlayerCoords pacman) (getPlayerOri pacman)
          gc = getPlayerCoords gho
          disL = distancia (leftcoords ori gc) pc
          disR = distancia (rightcoords ori gc) pc
          ori = (getPlayerOri gho)

-- | calcula as distância entre os pontos onde o fantasma pode ir e para onde quer ir e decide para onde vai
-- | Neste caso, o fantasma poderia ir à sua frente ou direita
calcBetterOrientationFR :: Player -> Player -> Orientation
calcBetterOrientationFR gho pacman
    | disF < disR = ori
    | disF > disR = rightorientation ori
    | otherwise = ori
    where casas = ( mod (getPlayerID gho) nPerfis)
          pc = ncasas casas (getPlayerCoords pacman) (getPlayerOri pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori gc) pc
          disR = distancia (rightcoords ori gc) pc
          ori = (getPlayerOri gho)

-- | calcula as distância entre os pontos onde o fantasma pode ir e para onde quer ir e decide para onde vai
-- | Neste caso, o fantasma poderia ir à sua frente ou esquerda
calcBetterOrientationFL :: Player -> Player -> Orientation
calcBetterOrientationFL gho pacman
    | disF < disL = ori
    | disF > disL = leftorientation ori
    | otherwise = ori
    where casas = ( mod (getPlayerID gho) nPerfis)
          pc = ncasas casas (getPlayerCoords pacman) (getPlayerOri pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori gc) pc
          disL = distancia (leftcoords ori gc) pc
          ori = (getPlayerOri gho)                    

-- | calcula as distância entre os pontos onde o fantasma pode ir e para onde quer ir e decide para onde vai
-- | Neste caso, o fantasma poderia ir à sua direita, esquerda ou à sua frente
calcBetterOrientations :: Player -> Player -> Orientation
calcBetterOrientations gho pacman
    | disL < disR && disL < disF = leftorientation ori
    | disR < disL && disR < disF = rightorientation ori
    | disF < disL && disF < disR = ori
    | disF < disL && disF == disR = ori
    | disF < disR && disF == disL = ori
    | disL < disF && disL == disR = leftorientation ori
    | otherwise = ori
    where casas = ( mod (getPlayerID gho) nPerfis)
          pc = ncasas casas (getPlayerCoords pacman) (getPlayerOri pacman)
          gc = getPlayerCoords gho
          disL = distancia (leftcoords ori gc) pc
          disR = distancia (rightcoords ori gc) pc
          disF = distancia (frontcoords ori gc) pc
          ori = (getPlayerOri gho)

-- | serve para ver para onde queremos que os nossas fantasma sigam
ncasas :: Int -> Coords -> Orientation -> Coords
ncasas casas (x,y) o
    | par && o == U = (x, y-casas)
    | par && o == D = (x, y+casas)
    | par && o == L = (x-casas, y)
    | par && o == R = (x+casas, y)
    | o == L = (x, y-casas)
    | o == R = (x, y+casas)
    | o == U = (x-casas, y)
    | o == D = (x+casas, y)
   where par = (mod casas 2 == 0)

-- | calcula as distância entre os pontos onde o fantasma pode ir e para onde quer ir fugir e decide para onde vai, ou seja quer a maior distancia entre dois pontos
-- | O Fantasma nesta função pode ir à sua frente ou à sua esquerda
calcBetterOrireverseFL :: Player -> Player -> Orientation
calcBetterOrireverseFL gho pacman
    | disF > disL = ori
    | disF < disL = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori gc) pc
          disL = distancia (leftcoords ori gc) pc
          ori = (getPlayerOri gho)

-- | calcula as distância entre os pontos onde o fantasma pode ir e para onde quer ir fugir e decide para onde vai, ou seja quer a maior distancia entre dois pontos
-- | O Fantasma nesta função pode ir à sua frente ou à sua direita
calcBetterOrireverseFR :: Player -> Player -> Orientation
calcBetterOrireverseFR gho pacman
    | disF > disR = ori
    | disF < disR = rightorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori gc) pc
          disR = distancia (rightcoords ori gc) pc
          ori = (getPlayerOri gho)

-- | calcula as distância entre os pontos onde o fantasma pode ir e para onde quer ir fugir e decide para onde vai, ou seja quer a maior distancia entre dois pontos
-- | O Fantasma nesta função pode ir à sua direita ou à sua esquerda
calcBetterOrireverseRL :: Player -> Player -> Orientation
calcBetterOrireverseRL gho pacman
    | disL > disR = leftorientation ori
    | disL < disR = rightorientation ori
    | otherwise = leftorientation ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disL = distancia (leftcoords ori gc) pc
          disR = distancia (rightcoords ori gc) pc
          ori = (getPlayerOri gho)
-- | calcula as distância entre os pontos onde o fantasma pode ir e para onde quer ir fugir e decide para onde vai, ou seja quer a maior distancia entre dois pontos
-- | O Fantasma nesta função pode ir à sua frente, à sua esquerda ou à sua frente
calcBetterOrisreverse :: Player -> Player -> Orientation
calcBetterOrisreverse gho pacman
    | disL > disR && disL > disF = leftorientation ori
    | disR > disL && disR > disF = rightorientation ori
    | disF > disL && disF > disR = ori
    | disF > disR && disF == disL = ori
    | disF > disL && disF == disR = ori
    | disL > disF && disL == disR = leftorientation ori    
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori gc) pc
          disL = distancia (leftcoords ori gc) pc
          disR = distancia (rightcoords ori gc) pc
          ori = (getPlayerOri gho)



-- | ve aonde está o pacman e tenta ir para o lado oposto
scatterMode :: State -> Int -> Play
scatterMode (State maze players level ) id
    | fst (getPlayerCoords player) == 0 = Move id L
    | fst (getPlayerCoords player) == length maze = Move id R
    | getPlayerCoords player == (meiox, meioy) || getPlayerCoords player == (meiox + 1, meioy) = Move id U
    | wallR == 0 && wallL == 0 && wallF == 0 = Move id (calcBetterOrisreverse player pacman)
    | wallF == 0 && wallR == 0 = Move id (calcBetterOrireverseFR player pacman)
    | wallF == 0 && wallL == 0 = Move id (calcBetterOrireverseFL player pacman)
    | wallR == 0 && wallL == 0 = Move id (calcBetterOrireverseRL player pacman)
    | wallF == 0 = Move id ori
    | wallR == 0 = Move id (rightorientation ori)
    | wallL == 0 = Move id (leftorientation ori)
    | otherwise = Move id (revertorientation ori)
    where player = getPlayerById players id
          ori = getPlayerOri player
          wallF = verificarwalls (revertorientation (ori)) maze (getPlayerCoords player)
          wallL = verificarwalls (revertorientation (leftorientation ori)) maze (getPlayerCoords player)
          wallR = verificarwalls (revertorientation (rightorientation ori)) maze (getPlayerCoords player)
          pacman = head $getPacman players
          meiox = div (length maze) 2
          meioy = div (length (head maze)) 2          

-- | vê aonde está o pacman e tenta ir para o sítio mais próximo dele
chaseMode :: State -> Int -> Play
chaseMode (State maze players level ) id
    | fst (getPlayerCoords player) == 0 = Move id L
    | fst (getPlayerCoords player) == length maze = Move id R
    | getPlayerCoords player == (meiox, meioy) || getPlayerCoords player == (meiox + 1, meioy) = Move id U
    | wallR == 0 && wallL == 0 && wallF == 0 = Move id (calcBetterOrientations player pacman)
    | wallF == 0 && wallR == 0 = Move id (calcBetterOrientationFR player pacman)
    | wallF == 0 && wallL == 0 = Move id (calcBetterOrientationFL player pacman)
    | wallR == 0 && wallL == 0 = Move id (calcBetterOrientationRL player pacman)
    | wallF == 0 = Move id ori
    | wallR == 0 = Move id (rightorientation ori)
    | wallL == 0 = Move id (leftorientation ori)
    | otherwise = Move id (revertorientation ori)
    where player = getPlayerById players id
          ori = getPlayerOri player
          wallF = verificarwalls ori maze (getPlayerCoords player)
          wallL = verificarwalls (leftorientation ori) maze (getPlayerCoords player)
          wallR = verificarwalls (rightorientation ori) maze (getPlayerCoords player)
          pacman = head $getPacman players
          meiox = div (length maze) 2
          meioy = div (length (head maze)) 2

-- | testes 
testemaze = State (generateMaze 30 30 30) [Ghost (GhoState (0,(9,9),0,D,0,3) Alive)] 1
          
testet99 :: State
testet99 = State (generateMaze 30 30 30) [(Ghost (GhoState (2,(4,1),1,R,1,1) Alive)),(Ghost (GhoState (3,(4,24),1,U,1,1) Dead)),(Ghost (GhoState (4,(4,24),1,U,1,1) Dead)),(Ghost (GhoState (5,(4,24),1,U,1,1) Dead))] 1
