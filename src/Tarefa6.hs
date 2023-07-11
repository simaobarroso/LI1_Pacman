-- |
-- = Realização da __/Tarefa6/__

module Tarefa6 where 

import Types
import Tarefa5
import Data.List 
import Tarefa2

{- |
A Tarefa6, tem como objetivo fazer que o pacman se mova automaticamente. Esta tarefa é basicamente o mesmo que a tarefa5, só que em vez de ser fantasmas a perseguir um pacman, é um pacman a perseguir / fugir do fantasma mais próximo do pacman e para ver o fantasma mais próximo usamos a função getGhost. 

    O Pacman, pode estar em 3 modos. O modo Dying, o modo Normal e o modo Mega. 
No modo Dying, nós simplesmente dissemos que não faz nada. No modo Normal e Mega, usamos a mesma lógica do que na Tarefa5, onde no modo Normal, o pacman foge do fantasma mais próximo e no modo Mega, perseguimos o fantasma mais próximo do pacman. 

    A dificuldade desta tarefa foi muito simplificada porque usamos a estrutura da Tarefa 5 para realizar o pensamento deste pacman. 

    Em conclusão, os problemas que tivemos ao construir esta tarefa, foram os mesmos que na Tarefa 5, por isso, quando detetamos um erro numa tarefa, também corrigíamos tal erro na outra. Nesta Tarefa, não usamos o indice para ter comportamentos diferentes, porque, só há um pacman, enquanto que na 5 teriamos de controlar multiplos fantasmas ao mesmo tempo. 
-}

-- | Função final desta Tarefa
bot :: Int -> State -> Maybe Play
bot x (State maze players level )
    | (getPacmanMode pacman == Mega) && (getGhostMode ghost == Dead) = megaChase x y
    | (getPacmanMode pacman == Mega) && (getGhostMode ghost == Alive) = normalScatter x y
    | getPacmanMode pacman == Normal = normalScatter x y
    | getPacmanMode pacman == Dying = Nothing
     where pacman = getPlayerById players x 
           y = (State maze players level )
           ghost = (getGhost players x)

-- | Quando o pacman estiver em megaChase irá usar esta função para perseguir o fantasma mais próximo
megaChase :: Int -> State -> Maybe Play
megaChase id (State maze players level )
    | snd (getPlayerCoords player) == 0 = Just (Move id R)
    | snd (getPlayerCoords player) == (length maze -1) = Just (Move id L) 
    | getPlayerCoords player == (meiox, meioy) || getPlayerCoords player == (meiox + 1, meioy) = Just (Move id U)
    | wallR == 0 && wallL == 0 && wallF == 0 && wallB == 0 = Just (Move id (calcShortOrisPac ghost pacmanplayer))
    | wallF == 0 && wallB == 0 && wallR == 0 = Just (Move id (calcShortOriPacFBR ghost pacmanplayer))
    | wallR == 0 && wallL == 0 && wallF == 0 = Just (Move id (calcShortOriPacRLF ghost pacmanplayer))
    | wallR == 0 && wallL == 0 && wallB == 0 = Just (Move id (calcShortOriPacRLB ghost pacmanplayer))
    | wallF == 0 && wallL == 0 && wallB == 0 = Just (Move id (calcShortOriPacFLB ghost pacmanplayer))
    | wallF == 0 && wallR == 0 = Just (Move id (calcShortOriPacFR ghost pacmanplayer))
    | wallF == 0 && wallB == 0 = Just (Move id (calcShortOriPacFB ghost pacmanplayer))
    | wallR == 0 && wallL == 0 = Just (Move id (calcShortOriPacRL ghost pacmanplayer))
    | wallR == 0 && wallB == 0 = Just (Move id (calcShortOriPacRB ghost pacmanplayer))
    | wallL == 0 && wallB == 0 = Just (Move id (calcShortOriPacLB ghost pacmanplayer))
    | wallF == 0 = Just (Move id ori)
    | wallR == 0 = Just (Move id (rightorientation ori))
    | wallL == 0 = Just (Move id (leftorientation ori))
    | wallB == 0 = Just (Move id (revertorientation ori))
    | otherwise = Nothing
    where player = getPlayerById players id
          ori = getPlayerOri player
          wallF = verificarwalls ori maze (getPlayerCoords player)
          wallL = verificarwalls (leftorientation ori) maze (getPlayerCoords player)
          wallR = verificarwalls (rightorientation ori) maze (getPlayerCoords player)
          wallB = verificarwalls (revertorientation ori) maze (getPlayerCoords player)
          pacman = head $getPacman players
          meiox = div (length maze) 2
          meioy = div (length (head maze)) 2
          ghost = (getGhost players id)
          pacmanplayer = (getPlayerById players id)

-- | Quando pacman estiver em modo normal, este irá usar esta função para fugir do fantasma mais próximo
normalScatter :: Int -> State -> Maybe Play
normalScatter id (State maze players level )
    | snd (getPlayerCoords player) == 0 = Just (Move id R) -- Just (Move id (calcBetterTunelL ghost pacmanplayer maze))
    | snd (getPlayerCoords player) == (length maze -1) = Just (Move id L) -- Just (Move id (calcBetterTunelL ghost pacmanplayer maze))
    | getPlayerCoords player == (meiox, meioy) || getPlayerCoords player == (meiox + 1, meioy) = Just (Move id U)
    | wallR == 0 && wallL == 0 && wallF == 0 && wallB == 0 = Just (Move id (calcBetterOrisPac ghost pacmanplayer))
    | wallF == 0 && wallB == 0 && wallR == 0 = Just (Move id (calcBetterOriPacFBR ghost pacmanplayer))
    | wallR == 0 && wallL == 0 && wallF == 0 = Just (Move id (calcBetterOriPacRLF ghost pacmanplayer))
    | wallR == 0 && wallL == 0 && wallB == 0 = Just (Move id (calcBetterOriPacRLB ghost pacmanplayer))
    | wallF == 0 && wallL == 0 && wallB == 0 = Just (Move id (calcBetterOriPacFLB ghost pacmanplayer))
    | wallF == 0 && wallR == 0 = Just (Move id (calcBetterOriPacFR ghost pacmanplayer))
    | wallF == 0 && wallB == 0 = Just (Move id (calcBetterOriPacFB ghost pacmanplayer))
    | wallR == 0 && wallL == 0 = Just (Move id (calcBetterOriPacRL ghost pacmanplayer))
    | wallR == 0 && wallB == 0 = Just (Move id (calcBetterOriPacRB ghost pacmanplayer))
    | wallL == 0 && wallB == 0 = Just (Move id (calcBetterOriPacLB ghost pacmanplayer))
    | wallF == 0 = Just (Move id ori)
    | wallR == 0 = Just (Move id (rightorientation ori))
    | wallL == 0 = Just (Move id (leftorientation ori))
    | wallB == 0 = Just (Move id (revertorientation ori))
    | otherwise = Nothing
    where player = getPlayerById players id
          ori = getPlayerOri player
          wallF = verificarwalls ori maze (getPlayerCoords player)
          wallL = verificarwalls (leftorientation ori) maze (getPlayerCoords player)
          wallR = verificarwalls (rightorientation ori) maze (getPlayerCoords player)
          wallB = verificarwalls (revertorientation ori) maze (getPlayerCoords player)
          pacman = head $getPacman players
          meiox = div (length maze) 2
          meioy = div (length (head maze)) 2
          ghost = (getGhost players id)
          pacmanplayer = (getPlayerById players id)

-- | Calcula o melhor movimento no tunel da direita
calcBetterTunelR :: Player -> Player -> Maze -> Orientation
calcBetterTunelR gho pacman maze
    | disB > disTunel = ori
    | disB < disTunel = R
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disB = distancia (pc) gc
          disTunel = distancia (otherside) gc
          ori = (getPlayerOri pacman)
          otherside = (fst pc,0)
          
-- | Calcula o melhor movimento no tunel da esquerda
calcBetterTunelL :: Player -> Player -> Maze -> Orientation
calcBetterTunelL gho pacman maze
    | dis > disTunel = ori
    | dis < disTunel = L
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          dis = distancia (pc) gc
          disTunel = distancia (otherside) gc
          ori = (getPlayerOri pacman)
          otherside = (fst pc, length maze -1)

-- | Diz quais são as coordenadas atrás do jogador (em perspetiva)
backcoords :: Orientation -> Coords -> Coords 
backcoords o (x,y) 
    | o == R = (x, y-1)
    | o == L = (x, y+1)
    | o == U = (x+1, y)
    | o == D = (x-1, y)  

{- | 

Esta secção do trabalho calcula a melhor orientacao para o nosso pacman ir, basicamente, escolhe a Orientacao para onde este se afastaria mais do fantasma.

R- (right), vê à sua direita
L- (left), vê à sua esquerda
F- (front), vê à sua frente
B- (back), vê as suas costas

A junção desta letras irá ser para comparar as situções
por exemplo:
calcBetterOriFL vai escolher a melhor orientacao para ir entre F e L, ou seja vai ver se a melhor opcao é ir à sua esquerda ou em frente.
-}

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função é visto todas as orientações à votla do pacman)
calcBetterOrisPac :: Player -> Player -> Orientation
calcBetterOrisPac gho pacman
    | disF > disB && disF > disR && disF > disL = ori
    | disB > disF && disB > disR && disB > disL = revertorientation ori
    | disR > disF && disR > disB && disR > disL = rightorientation ori
    | disL > disF && disL > disB && disL > disR = leftorientation ori
    | disF > disB && disF > disR && disF == disL = ori
    | disF > disB && disF > disL && disF == disR = ori
    | disF > disR && disF > disL && disF == disB = ori
    | disB > disF && disB > disL && disB == disR = rightorientation ori
    | disB > disF && disB > disR && disB == disL = leftorientation ori
    | disR > disF && disR > disB && disR == disL = rightorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disB = distancia (backcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente, atrás e à direita do pacman)
calcBetterOriPacFBR :: Player -> Player -> Orientation
calcBetterOriPacFBR gho pacman
    | disF > disB && disF > disR = ori
    | disB > disF && disB > disR = revertorientation ori
    | disR > disB && disR > disF = rightorientation ori
    | disF > disB && disF == disR = ori
    | disF > disR && disF == disB = ori
    | disR > disF && disR == disB = rightorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disB = distancia (backcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente, atrás e à esquerda do pacman)
calcBetterOriPacFLB :: Player -> Player -> Orientation
calcBetterOriPacFLB gho pacman
    | disF > disL && disF > disB = ori
    | disL > disF && disL > disB = leftorientation ori
    | disB > disF && disB > disL = revertorientation ori
    | disF > disB && disF == disL = ori
    | disF > disL && disF == disB = ori
    | disL > disF && disL == disF = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disB = distancia (backcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          ori = (getPlayerOri pacman)          

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente, à esquerda e à direita do pacman)
calcBetterOriPacRLF :: Player -> Player -> Orientation
calcBetterOriPacRLF gho pacman
    | disL > disR && disL > disF = leftorientation ori
    | disR > disL && disR > disF = rightorientation ori
    | disF > disL && disF > disR = ori
    | disF > disR && disF == disL = ori
    | disF > disL && disF == disR = ori
    | disL > disF && disL == disR = leftorientation ori    
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações atrás, à esquerda e à direita do pacman)
calcBetterOriPacRLB :: Player -> Player -> Orientation
calcBetterOriPacRLB gho pacman
    | disL > disR && disL > disB = leftorientation ori
    | disR > disL && disR > disB = rightorientation ori
    | disB > disL && disB > disR = revertorientation ori
    | disR > disL && disR == disB = rightorientation ori
    | disR > disB && disR == disL = rightorientation ori
    | disL > disR && disL == disB = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disB = distancia (backcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente e à esquerda do pacman)
calcBetterOriPacFL :: Player -> Player -> Orientation
calcBetterOriPacFL gho pacman
    | disF > disL = ori
    | disF < disL = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente e à direita do pacman)
calcBetterOriPacFR :: Player -> Player -> Orientation
calcBetterOriPacFR gho pacman
    | disF > disR = ori
    | disF < disR = rightorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente e atrás do pacman)
calcBetterOriPacFB :: Player -> Player -> Orientation
calcBetterOriPacFB gho pacman
    | disF > disB = ori
    | disF < disB = revertorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disB = distancia (backcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à esquerda e à direita do pacman)
calcBetterOriPacRL :: Player -> Player -> Orientation
calcBetterOriPacRL gho pacman
    | disL > disR = leftorientation ori
    | disL < disR = rightorientation ori
    | otherwise = leftorientation ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disL = distancia (leftcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações atrás e à direita do pacman)
calcBetterOriPacRB :: Player -> Player -> Orientation
calcBetterOriPacRB gho pacman
    | disB > disR = revertorientation ori
    | disB < disR = rightorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disB = distancia (backcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri gho)

-- | calcula a distância maior entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações atrás e à esquerda do pacman)
calcBetterOriPacLB :: Player -> Player -> Orientation
calcBetterOriPacLB gho pacman
    | disB > disL = revertorientation ori
    | disL > disB = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disB = distancia (backcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          ori = (getPlayerOri gho)

{- | 
Esta secção do trabalho faz o mesmo do que a anterior, só que neste caso, quer a distância mais perto (para perseguir o fantasma efecientemente)
 -}

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função é visto todas as orientações à votla do pacman)
calcShortOrisPac :: Player -> Player -> Orientation
calcShortOrisPac gho pacman
    | disF < disB && disF < disR && disF < disL = ori
    | disB < disF && disB < disR && disB < disL = revertorientation ori
    | disR < disF && disR < disB && disR < disL = rightorientation ori
    | disL < disF && disL < disB && disL < disR = leftorientation ori
    | disF < disB && disF < disR && disF == disL = ori
    | disF < disB && disF < disL && disF == disR = ori
    | disF < disR && disF < disL && disF == disB = ori
    | disB < disF && disB < disL && disB == disR = rightorientation ori
    | disB < disF && disB < disR && disB == disL = leftorientation ori
    | disR < disF && disR < disB && disR == disL = rightorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disB = distancia (backcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente, atrás e à direita do pacman)
calcShortOriPacFBR :: Player -> Player -> Orientation
calcShortOriPacFBR gho pacman
    | disF < disB && disF < disR = ori
    | disB < disF && disB < disR = revertorientation ori
    | disR < disB && disR < disF = rightorientation ori
    | disF < disB && disF == disR = ori
    | disF < disR && disF == disB = ori
    | disR < disF && disR == disB = rightorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disB = distancia (backcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente, atrás e à direita do pacman)
calcShortOriPacFLB :: Player -> Player -> Orientation
calcShortOriPacFLB gho pacman
    | disF < disB && disF < disL = ori
    | disB < disF && disB < disL = revertorientation ori
    | disL < disB && disL < disF = leftorientation ori
    | disF < disB && disF == disL = ori
    | disF < disL && disF == disB = ori
    | disL < disF && disL == disB = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disB = distancia (backcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente, à esquerda e à direita do pacman)
calcShortOriPacRLF :: Player -> Player -> Orientation
calcShortOriPacRLF gho pacman
    | disL < disR && disL < disF = leftorientation ori
    | disR < disL && disR < disF = rightorientation ori
    | disF < disL && disF <  disR = ori
    | disF < disR && disF == disL = ori
    | disF < disL && disF == disR = ori
    | disL < disF && disL == disR = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente, atrás e à direita do pacman)
calcShortOriPacRLB :: Player -> Player -> Orientation
calcShortOriPacRLB gho pacman
    | disL < disR && disL < disB = leftorientation ori
    | disR < disL && disR < disB = rightorientation ori
    | disB < disL && disB < disR = revertorientation ori
    | disR < disL && disR == disB = rightorientation ori
    | disR < disB && disR == disL = rightorientation ori
    | disL < disR && disL == disB = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disB = distancia (backcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente e à esquerda do pacman)
calcShortOriPacFL :: Player -> Player -> Orientation
calcShortOriPacFL gho pacman
    | disF < disL = ori
    | disF > disL = leftorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente e à direita do pacman)
calcShortOriPacFR :: Player -> Player -> Orientation
calcShortOriPacFR gho pacman
    | disF < disR = ori
    | disF > disR = rightorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente e atrás do pacman)
calcShortOriPacFB :: Player -> Player -> Orientation
calcShortOriPacFB gho pacman
    | disF < disB = ori
    | disF > disB = revertorientation ori
    | otherwise = ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disF = distancia (frontcoords ori pc) gc
          disB = distancia (backcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações à frente, atrás e à direita do pacman)
calcShortOriPacRL :: Player -> Player -> Orientation
calcShortOriPacRL gho pacman
    | disL < disR = leftorientation ori
    | disL > disR = rightorientation ori
    | otherwise = leftorientation ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disL = distancia (leftcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri pacman)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações atrás e à direita do pacman)
calcShortOriPacRB :: Player -> Player -> Orientation
calcShortOriPacRB gho pacman
    | disB < disR = revertorientation ori
    | disB > disR = rightorientation ori
    | otherwise = rightorientation ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disB = distancia (backcoords ori pc) gc
          disR = distancia (rightcoords ori pc) gc
          ori = (getPlayerOri gho)

-- | calcula a distância menor entre o fantasma e o pacman, dando a orientação. (Nesta função comparamos as orientações atrás e à esquerda do pacman)
calcShortOriPacLB :: Player -> Player -> Orientation
calcShortOriPacLB gho pacman
    | disB < disL = revertorientation ori
    | disB > disL = leftorientation ori
    | otherwise = leftorientation ori
    where pc = (getPlayerCoords pacman)
          gc = getPlayerCoords gho
          disB = distancia (backcoords ori pc) gc
          disL = distancia (leftcoords ori pc) gc
          ori = (getPlayerOri gho)


-- | Esta secção do trabalho tem como objetivo dar o fantasma mais proximo do nosso pacman

-- | Função final desta seccção
getGhost :: [Player] -> Int -> Player 
getGhost [] _ = error "No player found"
getGhost pl id | null pls1 = int
               | otherwise = ghostNear pccoords pls1 cod1 int
               where pc = eid id pl
                     pccoords = getPlayerCoords pc
                     pls = delete pc pl 
                     pls1 = tail pls
                     int = (head pls)
                     cod1 = distancia pccoords (getPlayerCoords int)

-- | Função auxiliar que compara as distancias entre os fantasma ao pacman
ghostNear :: Coords -> [Player] -> Float -> Player -> Player
ghostNear pccoords [h] n int | variavel = h
                             | otherwise = int
                             where variavel = (x) <= (n)
                                   x = distancia cd pccoords
                                   cd = (getPlayerCoords h)   
ghostNear pccoords (h:t) n  int | variavel = ghostNear pccoords t x h  
                                | otherwise = ghostNear pccoords t n int
                                where variavel = (x) <= (n)
                                      x = distancia cd pccoords
                                      cd = (getPlayerCoords h) 