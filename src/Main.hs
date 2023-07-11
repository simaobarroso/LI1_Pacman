module Main where

import Data.Time.Clock.POSIX
import Control.Monad.IO.Class
import UI.NCurses
import Types
import FileUtils
import Tarefa1
import Tarefa2
import Tarefa4
import Tarefa5
import Tarefa6
import Data.List

-- NÃO DEVEMOS EXPORTAR ESTE MAIN PARA ESTAS TAREFAS MAS SIM O CONTRÁRIO
-- Na tarefa 6 a estrategia tem de ser dinamica. PODEM OU NÃO TER DIFERENTES ESTRATEGIAS 
-- TAREFA 2 MOVIMENTAR OS PACMANS 
data Manager = Manager 
    {   
        state   :: State
    ,    pid    :: Int -- identificador de um jogador
    ,    step   :: Int -- quantas jogadas já se fez (movimentos que os jogadores fizeram). É aqui que se mede o tempo. Sempre que fizermos uma jogada adicionamos 1 *
    ,    before :: Integer -- tempo em que efetuamos a ultima jogada
    ,    delta  :: Integer -- tempo em que passou entre o instante em que estamos e o before 
    ,    delay  :: Integer -- 250 ms 
    } 

-- * STEP É IGUAL A STEP + 1 . TER ATENÇÃO AO TIME MEGA E A VELOCIDADE FANTASMA. METADE DA VELOCIDADE É JOGAR DE 2 EM 2 STEPS (STEP PAR) OU 500 MS.

loadManager :: Manager
loadManager = ( Manager (loadMaze "maps/1.txt") 0 0 0 0 defaultDelayTime )

-- completar. A cada tecla diz o que vai fazer 
-- ncurses teclas
-- usar isto para o pacman se mover o no terminaldiscuss neurology
-- aqui vamos ter de vamos ter de dar play. Pedir função ao Braga 
-- USAR UM CASE OF?? 
-- STEP ???????????????????????????????
updateControlledPlayer :: Key -> Manager -> Manager
updateControlledPlayer KeyUpArrow (Manager s p stp b dt dl)    = (Manager (otherway (Move p U) s) p stp b dt dl) 
updateControlledPlayer KeyDownArrow (Manager s p stp b dt dl)  = (Manager (otherway (Move p D) s) p stp b dt dl)
updateControlledPlayer KeyLeftArrow (Manager s p stp b dt dl)  = (Manager (otherway (Move p L) s) p stp b dt dl)
updateControlledPlayer KeyRightArrow (Manager s p stp b dt dl) = (Manager (otherway (Move p R) s) p stp b dt dl)

updateScreen :: Window  -> ColorID -> Manager -> Curses ()
updateScreen w a man =
                  do
                    updateWindow w $ do
                      clear
                      setColor a
                      moveCursor 0 0 
                      drawString $ show (state man)
                    render
     
currentTime :: IO Integer
currentTime = fmap ( round . (* 1000) ) getPOSIXTime

updateTime :: Integer -> Manager -> Manager
updateTime now (Manager s p stp b dt dl) = (Manager s p stp b (now-b) dl) -- TODO. TEMOS DE IMPLEMENTAR ESTA FUNÇÃO. ATUALIZAR O DELTA

resetTimer :: Integer -> Manager -> Manager
resetTimer now (Manager s p stp b dt dl) = (Manager s p stp now 0 dl)  -- TODO. TEMOS DE IMPLEMENTAR ESTA FUNÇÃO. VAI TER DE SER USAR NA NEXT FRAME. 

-- ERRO PISCAR. FAZER ESTAS FUNÇÕES. ATUALIZAR DELTA DO MANAGER. VAI USAR A PASS TIME. Vamos chamar a play
-- Chamar a play. 
nextFrame :: Integer -> Manager -> Manager
nextFrame now (Manager s@(State mz pls l) p stp b dt dl) = (Manager s1 p (stp+1) now 0 dl)
                                                        where s1 = passTime stp s -- play (Move p o1) s --  
                                                              pl1 = eid p pls
                                                              o1 = getPlayerOrientation pl1
                                                              {-rest = delete pl1 pls
                                                              doo = cometogether s1 pstm 
                                                              mz2 = maze s1
                                                              pstm = passTime stp (State mz2 rest l)-}

{-}
s1 = play (Move p o1) s
                                                              o1 = getPlayerOrientation (eid p pls)

-- COMPARA TEMPOS DE JOGO- JOGO MUITO LENTO?


-}
-- s1 = passTime stp s  -}                                                        
--passTime stp s
-- COLOR DEFAULT 
loop :: Window -> Manager -> Curses ()
loop w man@(Manager s pid step bf delt del ) = do 
  color_schema <- newColorID ColorBlack ColorWhite  10
  now <- liftIO  currentTime
  updateScreen w  color_schema man
  if ( delt > del )
    then loop w $ nextFrame now man
    else do
          ev <- getEvent w $ Just 0
          case ev of
                Nothing -> loop w (updateTime now man)
                Just (EventSpecialKey arrow ) -> loop w $ updateControlledPlayer arrow (updateTime now man)
                Just ev' ->
                  if (ev' == EventCharacter 'q') -- TECLA 'q' é para sair . PODEMOS ADICIONAR MAIS TECLAS. EXEMPLO 'p' para pausa. Ou '2' para trocar de player por exmplo.
                    then return ()
                    else loop w (updateTime now man)

main :: IO ()
main =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager

-- TESTE 44 

main2 :: IO ()
main2 =
  runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    loop w loadManager1

loadManager1 :: Manager
loadManager1 = ( Manager pcte2 0 0 0 0 defaultDelayTime )

pcte1:: State
pcte1 = State mazetest [Pacman ((PacState (0,(4,1),(1.5),U,1,1)) 0 Open Normal), (Ghost (GhoState (1,(4,0),1,L,1,1) Dead))] 1 

pcte :: State
pcte = State mazetest [Pacman ((PacState (0,(4,0),(1.5),L,1,1)) 10 Open Mega),(Ghost (GhoState (1,(7,12),(0.5),L,1,1) Dead))] 1
pcte2 :: State
pcte2 = State mazetest [(Ghost (GhoState (0,(4,0),1,U,1,1) Dead)),Pacman ((PacState (1,(7,2),(1.5),R,1,1)) 0 Open Normal)] 1
