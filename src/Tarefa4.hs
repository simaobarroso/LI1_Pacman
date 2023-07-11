-- |
-- = Realização da __/Tarefa4/__

module Tarefa4 where 

import Types
import Tarefa2
import Tarefa5
import Tarefa6

{-| Introdução : 
Nesta tarefa tinhamos de fazer com que todos os jogadores (players) se movessem no mesmo step, tal como no jogo original. Para isso tinhamos de implementar a função passTime.

Objetivos : 
Nesta tarefa decidimos usar uma função auxiliar com um acumular, assumindo portanto que a diferença entre os ids dos jogadores nunca seja maior do que um e que o primeiro id começava no 0.
Pensamos em 2 alternativas para implementar a velocidade : Ou verificar o modo do ghost ou a sua velocidade. Optamos pela velocidade para seguirmos o enunciado.
Na função passTime chamamos a função joga ao qual é dado os mesmos argumentos da passTime e um argumento extra (a length da lista de jogadores).
Com esta informação trabalhamos de trás para a frente, movendo primeiro o player com o id mais alto e decrescendo até chegar ao player com o id 0.
Aplicamos também nesta Tarefa a hipotese de chamar os bots dos ghost ou do pacman, tendo várias funções auxiliares para aplicar a chamada dos bots.

Conclusão:
Dado os teste que fizemos, esta Tarefa pareceu-nos concluida, uma vez que cumpre os objetivos propostos. Conseguimos com que em cada setp os players se movam todos e
que aqueles com velocidade 0.5 apenas se movam de duas em duas jogadas.
-}

defaultDelayTime = 250 -- 250 ms. Intervalo de tempo por cada jogada feita. 

-- | Jogadas por parte de todos os jogadores
passTime :: Int  -> State -> State
passTime x s@(State mz [] l) = s
passTime x s@(State mz pls l) = joga s x n
                                   where n = (length pls) - 1

-- | Função auxiliar da passTime. Faz a jogada de cada jogador durante o step em questão                                        
joga :: State -> Int -> Int -> State
joga s@(State mz pls l) x n | n < (0) = s
                            | (v05) && (odd x) = joga s x (n-1)  
                            | (ghst pmv) = (joga pls1 x (n-1))
                            | otherwise = joga (play (pcmmmv n s) s) x (n-1) -- (pcmmmv n s)
                            where 
                            pmv = eid n pls
                            v05 = ((getPlayerVelocity pmv) == (0.5))
                            pg = ghostPlay s
                            pls1 = play ghostmove s 
                            o = getPlayerOrientation pmv 
                            ghostmove = dcmove (pg) pmv

-- | Função auxiliar para implementar o bot do pacman. Tranforma a Maybe Play numa Play 
unMaybe :: Maybe Play -> Int -> Play
unMaybe x i = case x of
                   Nothing -> (Move i Null)
                   Just a -> a

-- | Função que implementa a jogada do bot do pacman
pcmmmv :: Int -> State -> Play
pcmmmv x (State maze players level) = unMaybe (bot x (State maze players level )) x

-- | Função auxiliar para o move dos fantasmas. Liga o move ao player
dcmove :: [Play] -> Player -> Play
dcmove [] _ = error "no move to do"
dcmove ((Move id o):t) p = if id == getPlayerID p then (Move id o) else dcmove t p





