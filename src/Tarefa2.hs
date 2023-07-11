-- |
-- = Realização da __/Tarefa2/__

module Tarefa2 where

import FileUtils
import Types
import Data.List
import System.IO.Unsafe

{-| Introdução : 
Nesta tarefa, tinhamos por objetivo implementar os movimentos dos jogadores. Numa primeira fase dos pacmans e numa segunda dos ghost.
Tinhamos portanto de implementar a função play.

Objetivos:
A nossa realização desta tarefa foi muito longa, pelo que é possivel ver várias abordagens e uma "evolução" nossa ao longo do trabalho.
Primeiramente para a mudança de direção, decidimos chamar uma função auxiliar que fazia isso pela play.
Mais tarde para, por exemplo, a comida grande, só utilizamos a lista de players. Ora isto é mais eficiente pois representa menos dados alimentados à função, pelo que
leva uma maior otimização.
Relativamente aos fantasmas, o seu comportamento não era muito diferente do dos pacmans, pelo que a sua implementação foi relativamente mais fácil.

Conclusão:
Para testar esta função fizemos uma quantidade anormal de testes, pois falhavamos em muitos dos testes dos professores. Algo que nos foi estranho, visto que nas nossas
implementações locais resultou, mas que nos ajudou a corrigir alguns erros.
Cremos que a função funciona dentro do que nos foi pedido para esta tarefa, mas apesar do nosso ponto de vista de que esta foi a tarefa mais dificil de implementar
devido às várias restrições, estamos confiantes no que foi feito aqui.  
-}

-- * Jogada mudar de orientação

-- | Compara a orientação do movimento. Condição para que faça a função otherway. 
getorientation :: Play  -- ^ jogada que fizemos
              ->  State   -- ^ Estado incial dos jogadores
              ->  Bool -- ^ Verdade se a orientação da jogada for diferente da do player a ser movimentado. Falso caso contrário
getorientation (Move id o) (State mz [] l ) = False
getorientation (Move id o) (State mz ((Pacman (PacState (i2,(x,y),v,o2,p,vd) m mouth mode)):n) l ) | id /= i2 = getorientation (Move id o) (State mz n l)
                                                                                                   | id == i2 && o /= o2 = True
                                                                                                   | otherwise = False
getorientation (Move id o) (State mz (((Ghost (GhoState (a,(x,y),c,d,e,v) gm))):n) l) | id == a && o /= d = True
                                                                                      | id /= a = getorientation (Move id o) (State mz n l)
                                                                                      | otherwise = False

-- | Muda a orientação do player jogado
otherway ::Play  -- ^ Jogada feita
        -> State  -- ^ Estado inicial dos players
        -> State -- ^ Estado final dos players depois de o player ser jogado
otherway _ s@(State mz [] l) = s
otherway (Move i1 o1) (State mz pc@[Pacman (PacState (i2,(x,y),v,o2,p,vd) m mouth mode)] l ) | i1 == i2 = (State mz [Pacman (PacState (i2,(x,y),v,o1,p,vd) m mouth mode)] l)
                                                                                             | i1 /= i2 = error "no player found"
otherway (Move i1 o1) (State mz ((Pacman (PacState (i2,(x,y),v,o2,p,vd) m mouth mode)):n) l ) | i1 == i2 = junta pc (State mz n l)
                                                                                              | i1 /= i2 = junta pc1 (otherway (Move i1 o1) (State mz n l))
                                                                                            where pc = (Pacman (PacState (i2,(x,y),v,o1,p,vd) m mouth mode))
                                                                                                  pc1 =(Pacman (PacState (i2,(x,y),v,o2,p,vd) m mouth mode))
otherway (Move i1 o1) (State mz (((Ghost (GhoState (a,(x,y),c,d,e,v) gm))):n) l) | i1 == a = junta gh (State mz n l)
                                                                                 | otherwise = junta gh1 (otherway (Move i1 o1) (State mz n l))
                                                                                 where gh = (Ghost (GhoState (a,(x,y),c,o1,e,v) gm))
                                                                                       gh1 = (Ghost (GhoState (a,(x,y),c,d,e,v) gm)) 



-- * Jogada mover por um túnel

-- | Testa se está na orientação do movimento do tunel
testasentido :: Maze -- ^ Labirinto dado
             -> Coords  -- ^ Coordenadas do player a ser movimentado
             -> Orientation -- ^ Orientação do player
             -> Bool -- ^ Verdade se está virado para o lado certo e se está nas coordenadas certas para o lado que vai virar. Falso caso contrário. 
testasentido mz (x,y) o | o == L && l = True 
                        | o == R && r = True
                        | otherwise = False
                       where l = dti mz (x,y)
                             r = dtf mz (x,y)

-- | Testa se o movimento pode ser executado no túnel da esquerda (da primeira coluna)
dti :: Maze  -- ^ Labirinto dado
    -> Coords -- ^ Coordenadas do player a ser movimentado
    -> Bool -- ^ Verdade se estiver na coordenadas do túnel da esquerda. Falso caso contrário
dti mz (x,y) | odd h = (x==l && y==0)
             | even h =  (x ==l && y==0 ) || (x ==(l-1) && y==0) 
             where h = length (mz)
                   l = (div h 2)

-- | Testa se o movimento pode ser executado no túnel da direita (da última coluna).
dtf :: Maze  -- ^ Labirinto dado
    -> Coords -- ^ Coordenadas do player a ser movimentado
    -> Bool -- ^ Verdade se estiver na coordenadas do túnel da direita. Falso caso contrário
dtf mz (x,y) | odd h = (x==l && y == t)
             | even h = (x ==l && y==t)||(x==(l-1) && y==t)
               where h = length (mz)
                     l = (div h 2)
                     t = length (head mz) -1

-- | Testa se o pacman está nas coordenadas onde possa atravessar o túnel
descobretunel :: Maze  -- ^ Labirinto dado
              -> Coords -- ^  Coordenadas do player que se vai movimentar
              -> Bool -- ^ Verdade se está nas coordenadas certas. Falso caso contrário
descobretunel mz (x,y) | odd h = (x==l && y==0) || (x==l && y == t)
                       | even h = ((x ==l && y ==0 ) || (x ==(l-1) && y ==0)) || ((x ==l && y==t)||(x==(l-1) && y==t))
                       where l = (div h 2) 
                             h = length (mz)
                             t = length (head mz) -1

-- | Testa se existe um fantasma do outro lado do túnel
fantOutroLado :: Maze  -- ^ labirinto dado
              -> Player  -- ^ Player movimentado
              -> [Player] -- ^ Lista de players
              -> Bool -- ^ Verdade se tiver um fantasma do outro lado, falso caso contrário
fantOutroLado _ _ [] = False
fantOutroLado mz n (h:t) | (ghst h) && outrolado = True
                         | otherwise = fantOutroLado mz n t 
                        where outrolado = testaoutrolado mz (getcoords n) (getcoords h)
                              
-- | Função auxiliar da anterior. Dada as coordenadas do player que vamos mover, confirma se existe fantasma do outro lado 
testaoutrolado :: Maze -- ^ Labirinto dado
               -> Coords -- ^ Coordenas do player movimentado
               -> Coords -- ^ Coordenadas do fantasma com que podemos embater
               ->  Bool -- ^ Verdade se estiver nas coordenadas do túnel, falso caso contrário
testaoutrolado mz (x1,y1) (x2,y2) | cond = True 
                                  | otherwise = False                      
                                where cond = (x1 == x2) && ((y1==0 && y2==l)||(y1==l && y2==0))
                                      l = (length (head mz)) - 1

-- | Caso o Player se movimente para um túnel
tunelmove :: Int  -- ^ ID do player
          -> Maze -- ^ Labirinto onde os players vão se movimentar 
          -> Orientation -- ^ Orientação do movimento do player
          -> [Player] -- ^ Lista de players inicial 
          -> [Player]  -- ^ Lista de players resultante
tunelmove _ _ _ [] = []
tunelmove id mz o (pl:pls) | testaID = h : pls
                           | otherwise = pl : t
                          where testaID = verificaID id pl 
                                h = tunel o mz pl
                                t = tunelmove id mz o pls  

-- | Altera o player que vai se vai movimentar pelo tunel 
tunel :: Orientation -- ^ Orientação do movimento
      -> Maze  -- ^  Labirinto onde se vão movimentar os players
      -> Player -- ^ Player que vai se movimentar no túnel
      -> Player  -- ^ Player resultante do movimento
tunel o mz (Pacman (PacState (a,(x,y),c,d,e,v)tm oc pm)) | y == 0 = Pacman (PacState (a,(x,h),c,d,e,v) tm oc pm) 
                                                         | y == h = Pacman (PacState (a,(x,0),c,d,e,v) tm oc pm) 
                                                         where h = length (head mz) -1  
tunel o mz (Ghost (GhoState (a,(x,y),c,d,e,v) gm)) | y == 0 = (Ghost (GhoState (a,(x,h),c,d,e,v) gm)) 
                                                   | y == h = (Ghost (GhoState (a,(x,0),c,d,e,v) gm)) 
                                                   where h = length (head mz) -1  
-- * Jogada embate com um fantasma

-- | Movimento de um player caso tenha um fantasma nas coordenadas para onde se vai mover  
ghostmove :: Play  -- ^ Jogada do player
          -> State  -- ^ Estado incial dos jogadores
          -> State -- ^ Estado resultante do movimento
ghostmove _ (State mz [] l) = (State mz [] l)
ghostmove (Move id o) (State mz (pl:pls) l) | (not testaID) && (not testaGhost) = junta (pl) (ghostmove (Move id o) (State m pls l)) -- Caso o primeiro não seja o player a mover 
                                            | peca == Food Big =  (State m (alpcgh id m (bigmoveLista id o (pl:pls))) l) -- Considerou-se o caso de haver uma Big Food e fantasma no mesmo sitio. Segundo indicação dos professores come-se primeiro a comida e depois dá-se o encontro com o fantasma 
                                            | peca == Food Little = (State m (alpcgh id m pecalittle) l) -- Acontece o mesmo de no caso anterior, exceto que desta vez é com comida pequena
                                            | otherwise = (State m (alpcgh id m playrs) l) -- restantes casos (ou seja Empty)
                                            where testaID = verificaID id pl
                                                  coords = getcoords pl1
                                                  peca = getpeca mz o coords
                                                  m = transEmptymap mz coords
                                                  playrs = quicksort ((movepl o pl1) :pl2)
                                                  testaGhost = ghst pl
                                                  pl1 = eid id (pl:pls) 
                                                  pl2 = delete pl1 (pl:pls)
                                                  pecalittle = quicksort((movepl o (aumentapnt pl1 1) ):pl2)
 
-- | Divide o caso os casos. Caso o pacman esteja no estado Mega vai chamar uma função, caso contrário outra.
alpcgh :: Int -- ^ ID do player
       -> Maze -- ^ Labirinto onde se vão movimentar os pacmans e fantasmas
       -> [Player] -- ^ Lista de players a ser alterada
       -> [Player] -- ^ Lista de players resultante da alteração
alpcgh _ _ [] = []
alpcgh id mz (pl:pls) =  case mega of 
                              True -> megapacman id mz (pl:pls) 
                              False -> matapacmanlista id (pl:pls)
                              where mega = testamega p
                                    p = eid id (pl:pls)   

-- | Testa o estado dos fantasmas do Labirinto (Se estão Dead ou Alive). Dá o resultado dos 2 casos
megapacman :: Int -- ^ ID do player
           -> Maze -- ^ Labirinto
           -> [Player] -- ^ Lista de players inicial
           -> [Player] -- ^ Lista de players alteradas
megapacman _ _ [] = []
megapacman id mz (pl:pls) | pecabig = quicksort ([ghostdead1 pl1 y1]  ++ ghsotvivo ++  (alterafantasmamorto id mz lista1) ++ (restantes pl2 coords1))
                          | fantmort = quicksort ([ghostdead1 pl1 y1]  ++ (alterafantasmamorto id mz lista1) ++ ghsotvivo ++ (restantes pl2 coords1))
                          | fantvm = quicksort ([matapacman (ghostdead1 pl1 y1)]  ++ ghsotvivo ++  (alterafantasmamorto id mz lista1) ++ (restantes pl2 coords1))
                          | otherwise = matapacmanlista id (pl:pls)
                          where fantmort = verifasefantamasmortos pl1 pl2 
                                pl1 = eid id (pl:pls)
                                pl2 = delete pl1 (pl:pls)
                                coords1 = getcoords pl1
                                lista1 = filtra pl2 coords1
                                y1 = length (alterafantasmamorto id mz lista1)
                                ghsotvivo = (lista1 ++ (pl1:(restantes pl2 coords1))) \\ (pl:pls)
                                fantvm = fantVivoMorto coords1 pl2
                                pecabig = testafoodbig mz coords1

-- | Testa se tem fantasmas vivos na casa para onde vamos movimentar
fantVivoMorto :: Coords -- ^ Player movimentado
              -> [Player] -- ^ Lista de players restantes 
              ->  Bool -- ^ Verdade se tiver, falso caso contrário
fantVivoMorto _ [] = False
fantVivoMorto coords pl2 | comp = True
                         | otherwise = False 
                        where comp = comparag (filtra pl2 coords)

-- | Diz se uma lista é composta por fantasmas ou não 
comparag :: [Player]  -- ^ Lista de players (neste caso só é usado para fantasmas)  
          -> Bool -- ^ Verdade se os ghosts mods de todos players for Dead. Falso caso contrário
comparag [Ghost (GhoState _ _)] = False
comparag playrslist = if (a) || (b) then False else True
                    where a = fantMortos playrslist
                          b = fantamavivo playrslist

fantMortos :: [Player] -> Bool
fantMortos [] = False
fantMortos (Ghost (GhoState _ a) : pls) = if a == Dead then True else fantMortos pls    
fantMortos (x:xs) = fantMortos xs  

fantamavivo :: [Player] -> Bool
fantamavivo [] = False
fantamavivo (Ghost (GhoState _ a) : pls) = if a == Alive then True else fantMortos pls  
fantamavivo (x:xs) = fantMortos xs  
                    


-- | Testa se há um fantasma para onde o player se vai mover
testghostnear :: Play  -- ^ Jogada a implementar
              -> State  -- ^ Estado dos players
              -> Bool -- ^ Se tiver um fantasma para onde o player se vai mover dá True. Caso contrário dá Falsp
testghostnear _ (State _ [] _) = False
testghostnear (Move id o) (State mz (pl:pls) l) = verifcaghost (getcoords (movepl o pl1)) pl2 
                                                where pl1 = eid id (pl:pls)
                                                      pl2 = delete pl1 (pl:pls)

-- | Verifica numas coordenadas se está lá um fantasma ou não 
verifcaghost :: Coords -- ^ Coordenadas de um player
             -> [Player] -- ^ Lista de jogadores analisada
             -> Bool -- ^ Verdade se houve algum fantasma. Falso caso contrário.
verifcaghost _ [] = False
verifcaghost (x,y) (pl:pls) | (coord == (x,y)) && (ghst pl) = True
                            | otherwise = verifcaghost (x,y) pls
                           where coord = (getcoords pl) 

-- | Altera cada fantasma. O fantasma vai para o meio do maze e o seu estado para para vivo
ghostdead :: Maze -> Player -> Player
ghostdead mz (Ghost (GhoState (a,b,h,d,e,v) q)) = Ghost (GhoState (a,(x,y),1,d,e,v) Alive)
                                                     where x = div (length mz) 2
                                                           y = div (length (head mz)) 2 
ghostdead _ x = x

-- * Jogada comer comida grande

-- | Caso o Player se movimente para um lugar onde está uma peça Big Food
bigmoveLista :: Int -- ^ ID do Player 
             -> Orientation -- ^ Orientação do movimentp 
             -> [Player] -- ^ Lista de PLayers que vai ser alterado
             -> [Player] -- ^ Resultado da alteração
bigmoveLista _ _ [] = []
bigmoveLista id o (pl:pls) | testapacman && (not testaID) = pl : t
                           | otherwise   = h : t
                           where testapacman = verificapacman pl
                                 testaID = verificaID id pl
                                 h = big o pl
                                 t = bigmoveLista id o pls

-- | Altera o player afetado pelo movimento
-- | O player (pacman) move-se, é lhe adicionado cinco pontos ao score e o seu estado muda para Mega. No caso dos fantasmas, é diminuida a velocidade e o seu estado passa para Dead   
big :: Orientation -- ^ Orientação do movimento
    -> Player -- ^ Player que vai ser alterado
    -> Player -- ^ Resultado da alteração
big o (Pacman (PacState (a,(x,y),c,op,e,f) tm oc pm)) = (Pacman (PacState (move o (a,(x,y),c,op,e+5,f) ) 37 oc Mega))   
big _ (Ghost (GhoState (a,b,v,d,e,f) gm)) = (Ghost (GhoState (a,b,0.5,d,e,f) Dead))


-- | Altera uma lista de fantasmas mortos caso sejam comidos 
alterafantasmamorto :: Int -- ^ ID do player
                   -> Maze  -- ^ Labirinto  
                   -> [Player] -- ^ Lista de players a ser alterada 
                   -> [Player] -- ^ Lista de players resultante
alterafantasmamorto _ _ [] = []
alterafantasmamorto id mz (pl:pls) = (ghostdead mz pl) : (alterafantasmamorto id mz pls)   

-- * Jogada comer comida pequena

-- | Função utilizada no caso de o pacman se mover para um "lugar" onde está uma comida pequena (Food Little)
littlemove :: Int -- ^ ID do player
           -> Orientation -- ^ orientação do movimento
           -> [Player]  -- ^ Lista inicial de players
           -> [Player] -- ^ Lista de players resultante (Afeta apenas o player movido)
littlemove _ _ [] = []
littlemove id o (pl:pls) | testaID = h : pls
                         | otherwise = pl : t
                        where testaID = verificaID id pl
                              h = little o pl 
                              t = littlemove id o pls 

-- | Altera o player afetado pelo movimento
-- | O player move-se e é lhe adicionado um ponto ao score  
little :: Orientation -- ^ Orientação do player 
       -> Player -- ^ Player que vai ser alterado
       -> Player -- ^ Player alterado 
little  o (Pacman (PacState ps tm oc pm)) = (Pacman (PacState (move o (aumentarpontuacao 1 ps)) tm oc pm))
little _ (Ghost (GhoState ps gm)) = (Ghost (GhoState ps gm))

-- ^ Jogada ir contra a parede

-- | Caso o Player se movimente para um lugar onde está uma peça Wall  
wallmove :: Int -- ^ ID do player
         -> Orientation  -- ^ Orientação do player
         -> [Player] -- ^ Lista de players inicial
         -> [Player] -- ^ Lista de players resultante
wallmove _ _ [] = []
wallmove id o (pl:pls) | testaID = h : pls
                       | otherwise = pl : t
                      where testaID = verificaID id pl
                            h = wall o pl
                            t = wallmove id o pls  

-- | Altera o Player que vai movimentar para a para
wall :: Orientation -- ^ Orientação do movimento
       -> Player -- ^ Player que vai ser alterado
       -> Player -- ^ Player resultante da alteração
wall o (Pacman (PacState (a,b,c,d,e,f) tm oc ps)) = (Pacman (PacState (a,b,c,o,e,f) tm oc ps)) 
wall _ x = x 

-- ^ Jogada ir para casa Empty

-- | Função utilizada no caso de o pacman se mover para um "lugar" onde está uma peça vazia (Empty)
emptymove :: Int  -- ^ ID do player
           -> Orientation -- ^ orientação do movimento
           -> [Player] -- ^ Lista de jogadores presente no labirinto 
           -> [Player] -- ^ Lista de jogadores alteradas (altera apenas o jogador afetado pela jogada)
emptymove _ _ [] = [] 
emptymove id o (pl:pls) | testaID = h : pls
                        | otherwise = pl : t 
                        where testaID = verificaID id pl
                              h = empty o pl
                              t = emptymove id o pls 

-- | Função que altera individualmente cada player do playstate  
-- | Nestes 2 casos fica igual pois se um jogador movimentar-se para um lugar Empty, nada é alterado 
empty :: Orientation  -- ^ Orientação do movimento
        -> Player -- ^ Player que vai ser alterado
        -> Player -- ^ Resultado da alteração
empty o (Pacman (PacState ps tm oc pm)) = (Pacman (PacState (move o ps) tm oc pm)) 
empty o (Ghost (GhoState ps gm)) = movepl o (Ghost (GhoState ps gm)) 

-- ^ Função play 
-- | Função a implentar nesta tarefa. Movimenta um pacman num labirinto
play1 :: Play -- ^ Jogada a implementar
       -> State  -- ^ State incial 
       -> State -- ^ Resultado da movimentação a um certo player
play1 (Move id o) s | deadpc = s
                    | getorirnt = openClosd id (otherway (Move id o) s) -- QUANDO A DIREÇÃO É DIFERENTE
                    | testetunel && testfant = openClosd id (s {maze = transEmptymap m coords, playersState = alpcgh id m (tunelmove id m o pl)})  -- TESTA SE É UM TUNEL E SE TEM LÁ UM FANTASMA. SE SIM MUDA AS COORDENADAS
                    | testetunel = openClosd id (s {maze = transEmptymap m coords, playersState = tunelmove id m o pl}) -- TESTA SE É UM TUNEL E SE SIM MUDA AS COORDENADAS
                    | testafantasma = openClosd id (ghostmove (Move id o) s) -- TESTA SE É UM FANTASMA 
                    | otherwise = case pecaseguinte of -- RESTANTES CASOS (PAREDE, EMPTY E COMIDAS)
                                  Wall -> openClosd id (s {playersState = wallmove id o pl} )
                                  Empty -> openClosd id (s {maze = transEmptymap m coords, playersState = emptymove id o pl})  
                                  Food Big -> openClosd id (s {maze = transEmptymap m coords, playersState = bigmoveLista id o pl} )
                                  Food Little -> openClosd id (s {maze = transEmptymap m coords, playersState = littlemove id o pl})
                                where pecaseguinte = getpeca m o coords
                                      coords = getcoords p
                                      p = eid id pl
                                      pl = playersState s
                                      m = maze s
                                      testetunel = (descobretunel m coords) && (testasentido m coords o)
                                      getorirnt = getorientation (Move id o) s
                                      l = level s  
                                      testafantasma = (testghostnear (Move id o) s) && ((getpeca m o coords) /= Wall) 
                                      testfant = fantOutroLado m p pl1
                                      pl1 = delete p pl
                                      deadpc = pcdead p

-- | Função Play. Move tanto pacman como ghost. Caso de o labirinto não ter players, renicia num novo labirinto.
play :: Play -> State -> State 
play m@(Move id o) s@(State mz pl l) | pontos = State mazetest pl (l+1)
                                     | testapacman = State mz1 pls l
                                     | otherwise = play2 m s
                                where testapacman = verificapacman pl0
                                      pls = quicksort pl4
                                      pl0 = eid id pl3 
                                      pl1 = pacmanMega pl0
                                      pl2 = fanstamaNormal pl3
                                      pl4 = pl1 : (delete pl0 pl2) 
                                      pl3 = playersState stt
                                      stt = play1 (Move id o) (State mz pl l)
                                      mz1 = maze stt
                                      pontos = contapontos (tail (init mz)) 
-- | Diz se um labitinto tem pontos ou não
contapontos :: Maze -> Bool
contapontos [] = True
contapontos ([]:t) = contapontos t
contapontos (h:t) | (head h) == Food Big || (head h) == Food Little = False
                  | otherwise =  contapontos ((tail h) : t)

-- | No caso de o movimento ser por parte dos fantasmas
play2 :: Play -> State -> State 
play2 (Move id o) s | orient = otherway (Move id o) s
                    | pcnext = s { playersState = pcgh id o mz pl} 
                    | testetunel && testapacman = s { playersState = pcght id mz (tunelmove id mz o pl)}
                    | testetunel = s { playersState = tunelmove id mz o pl}
                    | otherwise = case pecaseguinte of 
                                  Wall -> s
                                  Empty -> s {playersState = emptymove id o pl}
                                  Food Big -> s {playersState = emptymove id o pl}
                                  Food Little -> s {playersState = emptymove id o pl}
                                where pecaseguinte = getpeca mz o coords 
                                      mz = maze s 
                                      coords = getcoords p
                                      p = eid id pl
                                      pl = playersState s 
                                      pcnext = testpcmnext (Move id o) s
                                      testetunel = (((dti mz coords) && (o == L)) || ((dtf mz coords) && (o == R)))
                                      testapacman = pacmOutroLado mz p (delete p pl)
                                      orient = getorientation (Move id o) s

-- * Funções auxiliares
-- | Testa se tem um pacman do outro lado do túnel.
pacmOutroLado :: Maze  -- ^ labirinto dado
              -> Player  -- ^ Player movimentado
              -> [Player] -- ^ Lista de players
              -> Bool -- ^ Verdade se tiver um fantasma do outro lado, falso caso contrário
pacmOutroLado _ _ [] = False
pacmOutroLado mz n (h:t) | (not (ghst h)) && outrolado = True
                         | otherwise = pacmOutroLado mz n t 
                        where outrolado = testaoutrolado mz (getcoords n) (getcoords h)

-- | Altercação entre pacman e ghost 
pcght :: Int -> Maze -> [Player] -> [Player]
pcght _ _ [] = []
pcght id mz (pl:pls) | mega && ghostdied = quicksort ((ghostdead mz p):p2)
                     | otherwise = quicksort (p: (matapacmanlst pl3))
                     where mega = testapacmanega (pl:pls)
                           ghostdied = ((getPlayermod p) == Dead)
                           p = eid id (pl:pls)
                           coord = (getcoords p)
                           p2 = delete p (pl:pls)
                           pl3 = pcmans coord p2
                           pl4 = elimante (p:pl3) (pl:pls)
         
-- | testa se um pacman está a seguir
testpcmnext :: Play  -- ^ Jogada a implementar
              -> State  -- ^ Estado dos players
              -> Bool -- ^ Se tiver um fantasma para onde o player se vai mover dá True. Caso contrário dá Falsp
testpcmnext _ (State _ [] _) = False
testpcmnext (Move id o) (State mz (pl:pls) l) = verifcpc (getcoords (movepl o pl1)) pl2 
                                                where pl1 = eid id (pl:pls)
                                                      pl2 = delete pl1 (pl:pls)

-- | Verifica numas coordenadas se está lá um fantasma ou não 
verifcpc :: Coords -- ^ Coordenadas de um player
             -> [Player] -- ^ Lista de jogadores analisada
             -> Bool -- ^ Verdade se houve algum fantasma. Falso caso contrário.
verifcpc _ [] = False
verifcpc (x,y) (pl:pls) | (coord == (x,y)) && (not (ghst pl)) = True
                        | otherwise = verifcaghost (x,y) pls
                           where coord = (getcoords pl) 


-- | Altercação entre o ghost e o pacman
pcgh :: Int -> Orientation -> Maze -> [Player] -> [Player]
pcgh _ _ _ [] = []
pcgh id o mz (pl:pls) =  case mega of 
                              True -> case ghstdead of 
                                       True -> quicksort ((ghostdead mz (movepl o p)):p2)
                                       False -> quicksort ((movepl o p): (matapacmanlst pl3) ++  pl4)
                              False -> quicksort ((movepl o p): (matapacmanlst pl3) ++  pl4)
                              where mega = testapacmanega (pl:pls)
                                    p = eid id (pl:pls)
                                    coord = (getcoords (movepl o p))
                                    p2 = delete p (pl:pls)
                                    coords = getcoords (movepl o p) 
                                    pl3 = pcmans coords p2
                                    pl4 = elimante (p:pl3) (pl:pls)
                                    ghstdead = ((getPlayermod p) == Dead)

-- | testa se os pacmans estão nas mesmas coordenadas
pcmans :: Coords -> [Player] -> [Player]
pcmans _ [] = []
pcmans (x,y) (pl:pls) | (getcoords pl) == (x,y) = pl : pcmans (x,y) pls
                      | otherwise = pcmans (x,y) pls 

-- | elimina os players que não estão na segunda lista
elimante :: [Player] -> [Player] -> [Player]
elimante [] s = s 
elimante s [] = []
elimante (pl:pls) l | elem pl l = elimante pls (delete pl l)
                    | otherwise = elimante pls l 

-- | Muda o estado do pacman. Utilizada após a altercação com o ghost
matapacmanlst :: [Player] -> [Player]
matapacmanlst [] = []
matapacmanlst (x:xs) = matapacman x : matapacmanlst xs

-- | Testa se os pacmans estão no modo mega
pacmanMega :: Player -> Player 
pacmanMega (Pacman (PacState (a,b,c,d,e,f) tm oc ps)) | (tm <= 0) && (ps == Mega) = (Pacman (PacState (a,b,c,d,e,f) tm oc Normal))
                                                      | tm >= 1  = (Pacman (PacState (a,b,c,d,e,f) (tm-1) oc ps))
                                                      | otherwise = (Pacman (PacState (a,b,c,d,e,f) tm oc ps)) 
pacmanMega x = x 
-- | Coloca o fantasma em modo normal
fanstamaNormal :: [Player] -> [Player]
fanstamaNormal pls | testapacmega = pls
                   | otherwise = fantamasnormal pls 
                   where testapacmega = testapacmanega pls
-- | Testa se há algum pacman em modo mega
testapacmanega :: [Player] -> Bool
testapacmanega [] = False
testapacmanega ((Pacman (PacState _ _ _ ps)):n) = if ps == Mega then True else testapacmanega n 
testapacmanega (h:t) = testapacmanega t
-- | Torna o fantasma em modo normal
fantamasnormal :: [Player] -> [Player]
fantamasnormal [] = []
fantamasnormal ((Ghost (GhoState (a,b,c,d,e,f) gm)):n) = ((Ghost (GhoState (a,b,1,d,e,f) Alive))) : (fantamasnormal n)
fantamasnormal (h:t) = h : fantamasnormal t 
-- | confirma se o pacman está morto ou não
pcdead :: Player -> Bool
pcdead (Pacman (PacState _ _ _ ps)) = if ps == Dying then True else False
pcdead x = False 

-- | Ordena uma lista de players pelos seu IDs
quicksort :: [Player]  -- ^ Lista de players inicial 
            -> [Player]  -- ^ Lista de players ordenada
quicksort [] = []  
quicksort (pl:pls) =   
    let smallerSorted = quicksort [a | a <- pls, (compplayers a pl)]  
        biggerSorted = quicksort [a | a <- pls, (not (compplayers a pl))]  
    in  smallerSorted ++ [pl] ++ biggerSorted

-- | Compara os IDs de 2 players
compplayers :: Player  -- ^  1º player
             -> Player  -- ^ 2º player
             -> Bool -- ^ Verdade se o ID de um player for maior menor ou igual ao de outro
compplayers p1 p2 = (getPlayerID p1) <= (getPlayerID p2)

-- | Junta um player a um estado (Lista de players)
junta :: Player -- ^ Player inical 
        -> State    -- ^ Estado inicial de players
        -> State  -- ^ Estado de final de players com todos os players juntos
junta pl (State mz pls l) = (State mz (pl:pls) l)

-- | Função utilizada para descobrir qual é a peça presente na casa para qual o pacman se vai movimentar
getpeca :: Maze -- ^ Labirinto ao qual vamos buscar a peça
          -> Orientation -- ^ Orientação do pacman (dependendo da sua orientação, ou seja para onde o pacman se vai mover, a peça vai mudar)
          -> Coords  -- ^ coordenadas do pacman 
          -> Piece -- ^ peça resultante
getpeca m o (x,y) = case o of 
                         R -> (m!!x) !! (y+1)
                         L -> (m!!x) !! (y-1) 
                         U -> (m !! (x-1)) !! (y)
                         D -> (m !! (x+1)) !! (y)


-- | Verifica se o ID do player que vai ser movimentado (apenas do pacman) é correto ou não
verificaID :: Int -- ^ ID do player que vai ser movimentado
             -> Player  -- ^ Player que vai ser testado
             -> Bool -- ^  Verdade se o ID do jogador corresponder ao ID do player que vai ser movimentado). Falso caso contrário
verificaID id1 (Pacman (PacState (idp,_,_,_,_,_) _ _ _ )) = id1 == idp 
verificaID id1 (Ghost (GhoState (idp,_,_,_,_,_) _)) = id1 == idp

-- | Muda as coordenadas de um Player (ou seja "movimenta" um Player) de acordo com a orientação do movimento que vamos executar. 
-- | Se a orientação do Player for diferente da orientação dada, apenas se vai mudar a orientação do Player, mantendo-se nas mesma coordenadas
move :: Orientation -- ^ Orientação do movimento
       -> PlayerState -- ^ PlayerState que vai ser alterado
       -> PlayerState -- ^ PLayerState resultado
move o (a,(x,y),c,op,e,f)= case o == op of 
                                True -> case o of 
                                             R -> (a,(x,y+1),c,o,e,f)
                                             L -> (a,(x,y-1),c,o,e,f)
                                             U -> (a,(x-1,y),c,o,e,f) 
                                             D -> (a,(x+1,y),c,o,e,f)
                                False -> (a,(x,y),c,o,e,f)  

-- | Move o pacman
movepl :: Orientation  -- ^ Orientação do movimento
         -> Player  -- ^ Player que vai ser movimentado
         -> Player -- ^ Player resultante do movimento
movepl o (Pacman (PacState (a,(x,y),c,d,e,f) tm oc ps)) = case o of 
                                                           R -> (Pacman (PacState (a,(x,y+1),c,d,e,f) tm oc ps))
                                                           L -> (Pacman (PacState (a,(x,y-1),c,d,e,f) tm oc ps))
                                                           U -> (Pacman (PacState (a,(x-1,y),c,d,e,f) tm oc ps)) 
                                                           D -> (Pacman (PacState (a,(x+1,y),c,d,e,f) tm oc ps))
movepl o (Ghost (GhoState (a,(x,y),h,d,e,v) gm))        = case o of 
                                                           R -> (Ghost (GhoState (a,(x,y+1),h,d,e,v) gm))
                                                           L -> (Ghost (GhoState (a,(x,y-1),h,d,e,v) gm))
                                                           U -> (Ghost (GhoState (a,(x-1,y),h,d,e,v) gm))
                                                           D -> (Ghost (GhoState (a,(x+1,y),h,d,e,v) gm))

-- | Devolve as coordenadas de um player 
getcoords :: Player -- ^ Player dado
           -> Coords -- ^ Coordenadas do player
getcoords (Pacman (PacState (a,b,c,d,e,f) tm oc ps)) = b 
getcoords (Ghost (GhoState (a,b,c,d,e,f) gm)) = b

-- | Coloca no maze 
transEmptymap :: Maze -> Coords -> Maze
transEmptymap (h:t) (0,y) = (transEmptycor h y): t
transEmptymap (h:t) (x,y) = h : transEmptymap t (x-1,y)

-- | Substitui em cada corredor a peça de indice 0 por Empty
transEmptycor :: Corridor -> Int -> Corridor       
transEmptycor (h:t) 0 = Empty : t 
transEmptycor (h:t) x = h : transEmptycor t (x-1)

-- | Testa se um player é fantasma ou não
ghst :: Player -- ^ Player dado
     -> Bool -- ^ Verdade se for fantasma. Falso caso contrário
ghst (Ghost (GhoState _ _)) = True
ghst x = False

-- | Aumenta a pontução de um Player por um número variável 
aumentarpontuacao :: Int -- ^ Número pelo qual vamos aumentar a pontução
                  -> PlayerState -- ^ Estado do inicial do Player
                  -> PlayerState -- ^ Estado que é resultado do aumento de pontuação
aumentarpontuacao x (a,b,c,d,p,f) = (a,b,c,d,p+x,f)

-- | Multiplica a pontuação de um player por um número inteiro
aumentarpontuacao1 :: Int -> PlayerState -> Int -> PlayerState
aumentarpontuacao1 x (a,b,c,d,p,f) y = (a,b,c,d,p+(y*x),f)

-- | Verifica se numa lista de players todos estão mortos ou não
verifasefantamasmortos :: Player -- ^ Pacman que movemos (utilizamos para obter as suas coordenadas)
                       -> [Player] -- ^ Lista de Fantasmas mortos que existem na nas mesmas coordenadas do pacman
                       -> Bool -- ^ Verdade se não existem fantasmas vivos. Falso caso contrário
verifasefantamasmortos _ [] = True
verifasefantamasmortos pl pls | comp = True
                              | otherwise = False
                              where comp = compara (filtra pls coords) 
                                    coords = getcoords pl  

-- | Devolve a lista de players que não estão nas coordenadas do pacmana que movemos
restantes :: [Player] -- ^ Lista de todos os players existentes 
            -> Coords -- ^ Coordenadas do player que movemos
            -> [Player] -- ^ Lista de players que não estão nas coordenadas do player que movemos
restantes [] _ = []
restantes (pl:pls) (x,y) | getcoords pl == (x,y) = (restantes pls (x,y))
                         | otherwise = pl : restantes pls (x,y)

-- | Filtra os players existentes nas coordenada onde o jogador está.
filtra :: [Player] -- ^ Lista de todos players existentes
         -> Coords -- ^ Coordenadas do player que movemos
         -> [Player] -- ^ Lista de players que estão nas coordenadas do player que movemos
filtra [] _ = []
filtra (pl:pls) (x,y) | getcoords pl == (x,y) = pl : (filtra pls (x,y))
                      | otherwise = filtra pls (x,y)

-- | Compara os ghostmods  
compara :: [Player]  -- ^ Lista de players (neste caso só é usado para fantasmas)  
          -> Bool -- ^ Verdade se os ghosts mods de todos players for Dead. Falso caso contrário
compara [Ghost (GhoState _ n)] = if n == Alive then False else True
compara ((Ghost (GhoState _ n)):pls) | n == Alive = False
                                     | n == Dead = compara pls
compara x = False                                     

-- | Adiciona (10*y) pontos ao pacman que come os fantasmas.
-- | y é o número de fantasmas mortos que o pacman vai comer (Número de fantasmas mortos que estão naquelas coordenadas)
ghostdead1 :: Player -- ^ Pacman dado
             -> Int -- ^ Numero de fantasmas morto na casa para onde o pacman se move
             -> Player -- ^ Pacman com os pontos adicionados
ghostdead1 (Pacman (PacState (a,b,c,d,e,v) tm oc pm)) y = (Pacman (PacState (aumentarpontuacao1 10 (a,b,c,d,e,v) y) tm oc pm))

-- | Testa se um player está em estado Mega ou não
testamega :: Player -- ^ Player que vai ser testado
          -> Bool -- ^ Verdade se o estado do pacman for mega. Falso caso contrario
testamega (Pacman (PacState _ _ __ Mega)) = True
testamega _ = False

-- | Aumenta a pontução de um Player                                 
aumentapnt :: Player -- ^ Player dado
            -> Int -- ^ Número que vai ser somado à pontuação atual do player
            -> Player -- ^ Player Resultante
aumentapnt (Pacman (PacState (a,(x,y),c,d,e,v)tm oc pm)) k = (Pacman (PacState (a,(x,y),c,d,e+k,v)tm oc pm))
aumentapnt x y = x

-- | Muda o estado do pacman quando este embate num fantasma (este ultimo mantem-se no mesmo estado)
matapacman :: Player -- ^ Player recebido
           -> Player -- ^ Player 
matapacman (Pacman (PacState (a,b,c,d,e,v) tm oc ps)) | v == 0 = (Pacman (PacState (a,b,c,d,e,v) tm oc Dying))
                                                      | otherwise = (Pacman (PacState (a,b,c,d,e,v-1) tm oc ps)) 
matapacman x = x                                                       

-- | Para "matar" o pacman que se moveu contra um fantasma
matapacmanlista :: Int -- ^ ID do player
                -> [Player] -- ^ Lista de players
                -> [Player] -- ^ Lista de players modificada
matapacmanlista _ [] = []
matapacmanlista id (pl:pls) | testaID =  (matapacman pl) : pls
                            | otherwise = pl : (matapacmanlista id pls)
                           where testaID = verificaID id pl

-- | Testa se tem comida grande no sitio para onde o pacman se moveu
testafoodbig :: Maze  -- ^ Labirinto dado
             -> Coords -- ^ Coordenadas da peça movida
             -> Bool  -- ^ Verdade se está lá uma Food Big falso caso contrario 
testafoodbig mz (x,y) = if ((mz!!x) !! y) ==Food Big then True else False

-- | Função que encontra o id de um player 
eid :: Int -> [Player] -> Player 
eid _ [] = error "player not found"
eid id ((Pacman (PacState (a,b,c,d,e,v) tm oc ps)):n) | id == a = (Pacman (PacState (a,b,c,d,e,v) tm oc ps))
                                                      | otherwise = eid id n 
eid id ((Ghost (GhoState (a,b,h,d,e,v) gm)):t) | id == a = (Ghost (GhoState (a,b,h,d,e,v) gm))
                                               | otherwise = eid id t

-- | abre e fecha a boca do pacman
openClosd :: Int -> State -> State
openClosd id (State mz p l) = State mz (quicksort (pl:pl2)) l
                         where pl = changeOpenClosed pl1
                               pl1 = eid id p
                               pl2 = delete pl1 p 
-- | Função auxiliar da openClosd
changeOpenClosed :: Player -> Player
changeOpenClosed (Pacman (PacState (a,b,c,d,e,v) tm Open ps)) = (Pacman (PacState (a,b,c,d,e,v) tm Closed ps))
changeOpenClosed (Pacman (PacState (a,b,c,d,e,v) tm Closed ps)) = (Pacman (PacState (a,b,c,d,e,v) tm Open ps))
changeOpenClosed x = x


{- |
------------------------------------------------__NOTAS__------------------------------------------

->NESTE TRABALHO CONSIDEROU-SE QUE OS PACMANS ERAM "CRIADOS" EM LOCAIS ONDE HAVIAM PEÇAS VAZIAS, OU COMIDA PEQUENA OU COMIDA GRANDE, DAÍ QUE 
  APAGA SEMPRE AS PEÇAS (TIRANDO FANTASMAS) POR ONDE O PACMAN PASSA. 
  AO SER "CRIADO" NESTE LOCAL O PACMAN NÃO ABSORVE OS PONTOS DO PRÓPRIO LOCAL, APENAS RETIRA A PEÇA. 

-> NÃO TESTAMOS NEM CRIAMOS UMA FUNÇÃO QUE DIFERENCIE OS IDS, JÁ QUE SUPOMOS QUE SÃO DE ORDEM CRESCENTE E QUE SÃO DIFERENTES 

-> TIVEMOS VÁRIOS PROBLEMAS COM O CODEBOARD, DAÍ A EXTENSA QUANTIDADE DE TESTES LOCAIS QUE REALIZAMOS

-}
{- |
------------------------------------------------__DICIONÁRIO DE VARIÁVEIS__------------------------------------------

testaID -> Testa se o id do pacman que vamos movimentar corresponde ao do primeiro pacman da lista de players 
coords -> Coordenadas do player que vamos movimentar  
testetunel -> Testa se o player está nas condições certas para atravessar o túnel 
getorirnt -> Compara a orientação do movimento com o do pacman que vamos mover
pl1 -> Player que vamos mover
pl2 -> Lista de players retirando o player que vams mover
pecaseguinte -> peça que está na coordenadas para onde o pacman se vai mover
testafantasma -> testa se existe um fantas para as coordenadas para onde o pacman se vai mover
testfant -> Testa se existe um fantasma do outro lado do túnel 

-}

------------------------------------------------__TESTES LOCAIS__----------------------------------------------------
-- SERVEM PARA TESTAR SE A FUNÇÃO PLAY FUNCIONA CORRETAMENTE NAS DIVERSAS SITUAÇÕES, AJUDANDO NÃO SÓ A PERCEBER COMO A FUNÇÃO FUNCIONA MAS TAMBÉM A CORRIGIR POTENCIAIS ERROS 
-- ESTES LABIRINTOS APENAS SERVEM PARA EFEITOS DE TESTE JÁ QUE NÃO CUMPREM OS REQUESITOS NECESSESÁRIOS
-- DAI CADA LABIRINTO SER ADAPTADO PARA O DIFERENTES TESTES



-- | Labirinto para o teste 1 (beta)
maze1 :: Maze
maze1 = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Wall],
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Wall],
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Wall],
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Wall],
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Wall],
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],
        [Wall,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Wall],
        [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]

-- | Teste Beta 1 
pcs :: State
pcs = State maze1 [Pacman ((PacState (0,(6,13),1,R,1,1)) 0 Open Normal),(Ghost (GhoState (1,(6,14),1,U,1,1) Dead))] 1


---------------------------- 1º TESTE (MUDAR DE DIREÇÃO) -- FUNCIONA
-- | Maze mudar de direção
mazemd :: Maze 
mazemd = [[Wall,Wall,Wall,Wall],
          [Empty,Empty,Empty,Empty],
          [Empty,Empty,Empty,Empty],
          [Empty,Empty,Empty,Empty],
          [Wall,Wall,Wall,Wall]]

-- | Teste mudar de direção
pcsmd :: State 
pcsmd = State mazemd [Pacman ((PacState (0,(2,2),1,R,0,1)) 0 Open Normal), Pacman ((PacState (1,(3,3),1,L,0,1)) 0 Open Normal),(Ghost (GhoState (2,(1,2),1,U,0,1) Dead))] 1 


---------------------------- 2º Teste (COMER FOOD BIG E FOOD SMALL E EMPTY E TESTAR SE DEIXA A CASA VAZIA) -- FUNCIONA 
-- | Maze comer comida
mazecd :: Maze
mazecd = [[Wall,Wall,Wall,Wall],
          [Food Big,Food Big,Food Big,Food Big],
          [Food Little,Empty,Food Big,Food Big],
          [Food Little,Food Big,Empty,Food Little],  
          [Food Little,Food Little,Food Little,Food Little]]

-- | Teste comer comida
pcmcd :: State
pcmcd = State mazecd [Pacman ((PacState (0,(3,3),1,L,0,1)) 0 Open Normal),Pacman ((PacState (1,(2,2),1,R,0,1)) 0 Open Normal), (Ghost (GhoState (2,(1,2),1,U,0,1) Dead)), Pacman ((PacState (3,(4,2),1,R,0,1)) 0 Open Mega),Pacman ((PacState (4,(4,0),1,R,0,1)) 0 Open Normal)] 1 


---------------------------- 3º TESTE (PASSAR NO TÚNEL)
-- | Maze tunel par
mazetp :: Maze 
mazetp = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall], --0
        [Wall, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Wall],--1
        [Wall,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--2
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Wall],--3
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--4
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Empty], --5
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--6 (tunel)
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Empty],--7 (tunel)
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--8
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Empty],--9
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--10
        [Wall,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--11
        [Wall,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Wall],-- 12
        [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]] -- 13

-- | Maze tunel impar
mazeti :: Maze
mazeti = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall], --0
        [Wall, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Wall],--1
        [Wall,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--2
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Empty],--3
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--4
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Empty], --5
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--6 (tunel)
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Empty],--7 
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--8
        [Empty, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big, Empty],--9
        [Empty,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--10
        [Wall,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Empty],--11
        [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]] -- 12

-- | Teste tunel impar
pcmtpi :: State
pcmtpi = State mazetp [Pacman ((PacState (0,(3,0),1,L,1,1)) 0 Open Normal),Pacman ((PacState (1,(4,0),1,L,1,1)) 0 Open Normal), (Ghost (GhoState (2,(1,2),1,U,1,1) Dead)),Pacman ((PacState (3,(6,0),1,L,1,1)) 0 Open Normal),Pacman ((PacState (4,(7,0),1,L,1,1)) 0 Open Mega),Pacman ((PacState (5,(8,0),1,L,1,1)) 0 Open Normal),Pacman ((PacState (6,(10,0),1,L,1,1)) 0 Open Mega),Pacman ((PacState (7,(12,0),1,L,1,1)) 0 Open Dying)] 1

-- | Teste tunel par
pcmtpf :: State
pcmtpf = State mazetp [Pacman ((PacState (0,(3,14),1,R,1,1)) 0 Open Normal),Pacman ((PacState (1,(4,14),1,R,1,1)) 0 Open Normal), (Ghost (GhoState (2,(1,2),1,U,1,1) Dead)),Pacman ((PacState (3,(6,14),1,R,1,1)) 0 Open Normal),Pacman ((PacState (4,(7,14),1,R,1,1)) 0 Open Mega),Pacman ((PacState (5,(8,14),1,R,1,1)) 0 Open Normal),Pacman ((PacState (6,(10,14),1,R,1,1)) 0 Open Mega),Pacman ((PacState (7,(12,14),1,R,1,1)) 0 Open Dying)] 1


--- TESTAR MELHOR (MAS FUNCIONA)

---------------------------- 4º TESTE (COMER FANTASMA MORTO)
-- | Maze teste normal e mega
mazecfm :: Maze 
mazecfm = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
           [Wall, Food Little, Food Little, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Wall],
           [Wall, Food Little, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Wall],
           [Wall, Food Little, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Wall],
           [Wall,Empty,Food Big, Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Food Big,Empty,Wall],
           [Empty,Empty,Food Little,Empty, Wall, Wall, Wall, Empty, Empty, Wall, Wall, Wall,Empty,Food Big,Empty,Empty],
           [Wall,Empty,Food Little,Empty, Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall,Empty,Food Big,Empty,Wall],
           [Wall,Empty,Food Little,Empty, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall,Empty,Food Big,Empty,Wall],
           [Wall,Empty,Food Little,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Food Big,Empty,Wall],
           [Wall, Food Little,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Wall],
           [Wall, Food Little, Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Wall],
           [Wall,Food Little,  Empty, Empty, Empty, Empty, Empty, Empty ,Food Big,Food Big,Empty,Food Big,Food Big,Food Little,Food Big,Wall],                   
           [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]] 
-- | Teste Embate Fantasma
pcmcfm :: State
pcmcfm = State mazecfm [Pacman ((PacState (0,(1,1),1,R,0,1)) 0 Open Mega), Pacman ((PacState (1,(11,4),1,R,0,1)) 0 Open Mega),(Ghost (GhoState (2,(1,2),1,L,0,1) Dead)),(Ghost (GhoState (3,(11,5),1,L,0,1) Dead))] 1


---------------------------- 5º TESTE (EMBATE FANTASMA VIVO)
-- | Teste embate fantasma vivo
pcmcfv :: State
pcmcfv = State mazecfm [Pacman ((PacState (0,(1,0),1,R,1,1)) 0 Open Normal), Pacman ((PacState (1,(2,0),1,R,1,0)) 0 Open Mega),(Ghost (GhoState (2,(1,1),1,U,1,1) Alive)),(Ghost (GhoState (3,(2,1),1,U,1,1) Alive))] 1

------------------------ 6º TESTE (TESTE DE TODAS AS SITUAÇÕES) 
-----------TESTE PROFESSORES
{-}
#########################
#o.....................o#
#.######.........######.#
} #    #.### ###.#    #. 
 .#    #.#  M  #.#    #. 
#.######.#######.######.#
#o...................?.o#
#########################
-}
-- | Maze testes professores
mazetest :: Maze
mazetest = [[Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
            [Wall,Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Big, Wall],
            [Wall,Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Big, Wall],
            [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little, Food Little, Food Little, Food Little,Food Little, Food Little, Food Little, Food Little, Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
            [Empty,Empty,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Wall,Wall,Empty,Wall,Wall,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Empty],
            [Wall,Empty,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Empty,Wall,Food Little,Wall,Empty,Empty,Empty,Empty,Wall,Food Little,Wall],
            [Wall,Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little, Wall,Wall,Wall,Wall,Wall,Wall,Wall, Food Little,Wall,Wall,Wall,Wall,Wall,Wall,Food Little,Wall],
            [Wall,Food Big, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little, Food Little,Food Big, Wall],
            [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]
-- | Teste Fantasma tunel outro lado          
testet :: State
testet = State mazetest [Pacman ((PacState (0,(4,0),1,L,1,1)) 0 Open Mega),(Ghost (GhoState (2,(4,1),1,R,1,1) Alive)),(Ghost (GhoState (3,(4,24),1,U,1,1) Dead)),(Ghost (GhoState (4,(4,24),1,U,1,1) Dead)),(Ghost (GhoState (5,(4,24),1,U,1,1) Dead))] 1
-- | teste exemplo professores
pcteste :: State
pcteste = State mazetest [Pacman ((PacState (0,(4,0),1,L,1,1)) 0 Open Normal),(Ghost (GhoState (2,(7,20),1,U,1,1) Alive))] 1
-- | teste embate fantasma 1
pcteste1:: State
pcteste1 = State mazetest [Pacman ((PacState (0,(4,0),1,D,1,1)) 0 Open Mega),(Ghost (GhoState (2,(5,0),1,U,1,1) Alive))] 1 
-- | teste embate fantasma 2
pcteste2 :: State
pcteste2 = State mazetest [(Ghost (GhoState (0,(7,20),1,U,1,1) Dead)),Pacman ((PacState (1,(7,21),1,L,1,1)) 0 Open Normal),(Ghost (GhoState (2,(7,20),1,U,1,1) Dead))] 1
-- | teste embate fantasma 3
teste1 :: State
teste1 = State mazetest [Pacman ((PacState (0,(7,21),1,L,1,1)) 0 Open Mega),(Ghost (GhoState (2,(7,20),1,U,1,1) Dead)),(Ghost (GhoState (3,(7,20),1,U,1,1) Alive))] 1
-- | teste embate fantasma 4 
teste2 :: State
teste2 = State mazetest [Pacman ((PacState (0,(7,21),1,L,1,1)) 0 Open Mega),(Ghost (GhoState (1,(7,20),1,U,1,1) Dead)),(Ghost (GhoState (2,(7,20),1,U,1,1) Dead)),(Ghost (GhoState (3,(7,20),1,U,1,1) Dead))] 1
-- | teste embate fantasma 5
teste3 :: State 
teste3 = State mazetest [(Ghost (GhoState (0,(7,20),1,U,1,1) Alive)), Pacman ((PacState (1,(7,21),1,L,1,1)) 0 Open Mega), (Ghost (GhoState (2,(7,20),1,U,1,1) Dead)),(Ghost (GhoState (3,(7,20),1,U,1,1) Dead))] 1
-- | teste embate fantasma 6
teste4 :: State
teste4 = State mazetest [(Ghost (GhoState (0,(7,1),1,U,1,1) Alive)), Pacman ((PacState (1,(7,2),1,L,1,1)) 0 Open Mega), (Ghost (GhoState (2,(7,1),1,U,1,1) Dead)),(Ghost (GhoState (3,(7,1),1,U,1,1) Dead))] 1
-- | teste embate fantasma 7
teste5 :: State 
teste5 = State mazetest[(Ghost (GhoState (2,(7,20),1,U,1,1) Alive)),Pacman ((PacState (1,(4,0),1,L,1,1)) 0 Open Normal)] 1
-- | teste embate fantasma 8
testet1 :: State
testet1 = State mazetest [Pacman ((PacState (5,(4,0),1,L,1,1)) 0 Open Mega),(Ghost (GhoState (4,(4,24),1,U,1,1) Alive)),(Ghost (GhoState (3,(4,24),1,U,1,1) Dead)),(Ghost (GhoState (2,(4,24),1,U,1,1) Dead)),(Ghost (GhoState (1,(1,24),1,U,1,1) Dead))] 1
-- | Teste final com todas as situações
testefinal :: State
testefinal = State mazetest [Pacman ((PacState (0,(5,0),1,L,1,1)) 0 Open Mega),Pacman ((PacState (1,(5,0),1,L,1,1)) 0 Open Normal),(Ghost (GhoState (2,(5,24),1,U,1,1) Dead)), Pacman ((PacState (3,(4,24),1,L,1,1)) 0 Open Normal), (Ghost (GhoState (4,(5,6),1,U,1,1) Alive)),Pacman ((PacState (5,(5,5),1,U,1,1)) 0 Open Mega)] 1


t1 = [Pacman ((PacState (0,(7,21),1,L,1,0)) 0 Open Mega),(Ghost (GhoState (2,(7,20),1,U,1,1) Dead)),(Ghost (GhoState (3,(7,20),1,U,1,1) Alive))]