{- |
Esta Tarefa tinha como objetivo efetuar uma jogada. Em comparação à Tarefa anterior, esta foi consideralmente mais difícil
-}

-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa2_2019li1g042 where

import LI11920
import Tarefa1_2019li1g042 
import Tarefa0_2019li1g042

-- * Testes

-- | Testes unitários da Tarefa 2.
--
-- Cada teste é um triplo (/identificador do 'Jogador'/,/'Jogada' a efetuar/,/'Estado' anterior/).

-- | Mapas base e Jogadores para usar nos testes
mapa1 = [[Recta Terra 0, Recta Terra 0, Recta Terra 0], [Recta Terra 0, Recta Terra 0, Recta Terra 0], [Recta Terra 0, Recta Terra 0, Recta Terra 0]]
mapa2 = [[Recta Terra 0, Rampa Relva 0 1, Recta Terra 1], [Recta Terra 0, Recta Relva 0, Rampa Terra 0 1], [Recta Terra 0, Rampa Relva 0 2, Recta Lama 2]]
mapa3 = [[Recta Terra 0, Recta Terra 0, Recta Terra 0], [Recta Terra 0, Rampa Terra 0 1, Recta Terra 1]]
mapa4 = [[Recta Terra 0, Rampa Terra 0 1], [Recta Terra 0, Rampa Terra 0 1], [Recta Terra 0, Recta Terra 0]]
jogador1 = [(Jogador 1 1 1 1 (Chao False)), (Jogador 1 1.5 1 0 (Chao True))]
jogador2 = [(Jogador 1 2 3 4 (Chao True)), (Jogador 1 2.9 1 0 (Chao False))]
jogador3 = [(Jogador 1 1 1 1 (Chao True)), (Jogador 1 1 1 1  (Ar 2 15 3))]
jogador4 = [(Jogador 0 1 1 1 (Chao True)), (Jogador 0 1.6 2 3 (Chao False))]
jogador5 = [(Jogador 1 1 1 1 (Chao True)), (Jogador 1 1 1 1  (Ar 2 (-80) 3))]
jogador6 = [(Jogador 1 1 1 1 (Chao True)), (Jogador 1 1 1 1  (Ar 2 80 3))]
jogador7 = [(Jogador 1 1 1 1 (Chao True)), (Jogador 1 0.3 2 2 (Chao True))]
jogador8 = [(Jogador 0 1 1 1 (Chao True)), (Jogador 1 1.6 2 3 (Chao False))]
jogador9 = [(Jogador 1 2 3 4 (Chao True)), (Jogador 1 2.9 1 1 (Chao False))]
testesT2 :: [(Int,Jogada,Estado)]
testesT2 = [(1, (Movimenta C) , (Estado mapa2 jogador1)) , (1, (Movimenta C), (Estado mapa2 jogador2)) , (1, (Movimenta C) , (Estado mapa1 jogador1)) , (1, (Movimenta C) , (Estado mapa3 jogador1)) , (1, (Movimenta C), (Estado mapa2 jogador3)) , (1, (Movimenta C), (Estado mapa2 jogador4))
           ,(1, (Movimenta B) , (Estado mapa1 jogador1)) , (1, (Movimenta B), (Estado mapa2 jogador4)) , (1, (Movimenta B) , (Estado mapa3 jogador4)) , (1, (Movimenta B) , (Estado mapa3 jogador3)) , (1, (Movimenta B) ,(Estado mapa2 jogador1)) , (1, (Movimenta B), (Estado mapa1 jogador4)) , (1, (Movimenta B) , (Estado mapa3 jogador1)) , (1, (Movimenta B) , (Estado mapa4 jogador8)) 
           ,(1, (Movimenta E) , (Estado mapa1 jogador1)) , (1, (Movimenta E), (Estado mapa1 jogador3)) , (1, (Movimenta E) , (Estado mapa1 jogador5)) , (1, (Movimenta E) , (Estado mapa1 jogador6))
           ,(1, (Movimenta D) , (Estado mapa1 jogador1)) , (1, (Movimenta D), (Estado mapa1 jogador3)) , (1, (Movimenta D) , (Estado mapa1 jogador5)) , (1, (Movimenta D) , (Estado mapa1 jogador6))
           ,(1, (Dispara)     , (Estado mapa1 jogador1)) , (1, (Dispara)    , (Estado mapa1 jogador2)) , (1, (Dispara)     , (Estado mapa1 jogador7)) , (1, (Dispara)     , (Estado mapa3 jogador2)) , (1, (Dispara)     ,(Estado mapa1 jogador2)) , (1, (Dispara)    , (Estado mapa1 jogador4)) , (1, (Dispara)     , (Estado mapa3 jogador9))
           ,(1, (Acelera)     , (Estado mapa1 jogador1)) , (1, (Acelera)    , (Estado mapa1 jogador2)) , (1, (Acelera)     , (Estado mapa1 jogador5))
           ,(1, (Desacelera)  , (Estado mapa1 jogador1)) , (1, (Desacelera) , (Estado mapa1 jogador2)) , (1, (Desacelera)  , (Estado mapa1 jogador5))]
        
-- * Funções principais da Tarefa 2.


-- | Efetua uma jogada.
jogada :: Int -- ^ O identificador do 'Jogador' que efetua a jogada
       -> Jogada -- ^ A 'Jogada' a efetuar.
       -> Estado -- ^ O 'Estado' anterior.
       -> Estado -- ^ O 'Estado' resultante após o jogador efetuar a jogada. 


jogada n (Acelera) (Estado mapa ((Jogador pJ dJ vJ cJ eJ):  xs)) =  Estado mapa (acelarar n ((Jogador pJ dJ vJ cJ eJ):xs)) 
jogada n Desacelera  (Estado mapa ((Jogador pJ dJ vJ cJ eJ):xs))= Estado mapa (desacelera n ((Jogador pJ dJ vJ cJ eJ):xs))
jogada n Dispara (Estado mapa ((Jogador pJ dJ vJ cJ eJ):    xs)) = Estado (usarColaMapa n ((Jogador pJ dJ vJ cJ eJ):xs) mapa) (usarCola n ((Jogador pJ dJ vJ cJ eJ):xs))
jogada n (Movimenta E) (Estado mapa (x: xs)) = Estado mapa (rodarNoAr n (x:xs) E)     
jogada n (Movimenta D) (Estado mapa (x: xs)) = Estado mapa (rodarNoAr n (x:xs) D)     
jogada n (Movimenta C) (Estado mapa ((Jogador pJ dJ vJ cJ eJ): xs)) = Estado mapa (trocaPista n ((Jogador pJ dJ vJ cJ eJ):xs) mapa C)
jogada n (Movimenta B) (Estado mapa ((Jogador pJ dJ vJ cJ eJ): xs)) = Estado mapa (trocaPista n ((Jogador pJ dJ vJ cJ eJ):xs) mapa B)


-- A função 'posicaoJogador' determina a peça em que o Jogador está tendo em conta a pista e a distância dele
posicaoJogador :: Jogador -> Mapa -> Peca
posicaoJogador (Jogador pJ dJ vJ cJ eJ) mapa = encontraPosicaoMatriz ((fromIntegral pJ),floor dJ) mapa

-- |A função 'anguloRampa' calcula o ângulo de uma 'Rampa' e dá 0 quando é uma 'Recta' 
anguloRampa :: Peca -> Double
anguloRampa (Rampa z x y) = (atan (fromIntegral (y-x)))
anguloRampa (Recta z x) = 0

-- |A função 'alturaDoJogador' calcula a altura do 'Jogador' num certo ponto numa 'Rampa' ou 'Recta'
alturaDoJogador :: Peca -> Jogador -> Double
alturaDoJogador (Rampa tipo x y) (Jogador pJ dJ vJ cJ eJ) = ((tan (anguloRampa (Rampa tipo x y))) * (dJ - (fromIntegral (floor dJ))) + fromIntegral x)
alturaDoJogador (Recta tipo x) (Jogador pJ dJ vJ cJ eJ) = fromIntegral x

-- |A função 'diferencaAltura' calcula a diferenca de altura entre duas 'Pecas'
diferencaAltura :: Jogador -> Peca -> Peca  -> Double
diferencaAltura a@(Jogador pJ dJ vJ cJ eJ) (peca1) (peca2) = (alturaDoJogador (peca1) a ) - (alturaDoJogador (peca2) a)

-- | A função 'trocaPista' troca a 'Pista' do 'Jogador' (ou não, dependendo dos casos) utilizando as funções anteriores
trocaPista ::  Int -> [Jogador]  -> Mapa -> Direcao -> [Jogador]
trocaPista n [] mapa direcao = []
trocaPista 0 b@((Jogador pJ dJ vJ cJ (Ar altura inclinacao gravidade)):xs) mapa direcao =  b   
trocaPista 0 b@((Jogador pJ dJ vJ cJ (Morto x)) :xs) mapa direcao = b                                                                                                                           --  Caso o 'Jogador' esteja no ar, ele não vai trocar de pista, mantendo-se igual
trocaPista 0 b@((Jogador 0 dJ vJ cJ eJ):xs) mapa C = (Jogador 0 dJ vJ cJ eJ) :xs                                                                                                                --  Se se movimenta para cima o 'Jogador' não troca de 'Pista'
trocaPista n b@((Jogador 0 dJ vJ cJ eJ):xs) mapa C = (Jogador 0 dJ vJ cJ eJ) :trocaPista (n-1) xs mapa C                                                                                        --  A função faz recursividade para encontrar o Jogador pretendido quando a pista é 0
trocaPista 0 (b@(Jogador 0 dJ vJ cJ eJ):xs) mapa B                                                                                                                                              --  'TrocaPista' para movimentar para baixo quando a pista é 0
    | diferencaAltura (Jogador 0 dJ vJ cJ eJ) (posicaoJogador (Jogador 0 dJ vJ cJ eJ) mapa) (posicaoJogador (Jogador 1 dJ vJ cJ eJ) mapa) < -(0.2) =  (Jogador 0 dJ 0 cJ (Morto 1.0)) :xs       --  Se a diferença de altura for menor que (-0.2) o 'Jogador' morre
    | diferencaAltura (Jogador 0  dJ vJ cJ eJ) (posicaoJogador (Jogador 0  dJ vJ cJ eJ) mapa) (posicaoJogador (Jogador 1 dJ vJ cJ eJ) mapa) <= 0.2 =  (Jogador 1 dJ vJ cJ eJ) :xs               --  Se a diferença de altura for menor que  (0.2) o 'Jogador' troca de pista  
    | otherwise = (Jogador 1 dJ vJ cJ (Ar (alturaDoJogador (posicaoJogador b mapa) b) ((anguloRampa (posicaoJogador b mapa)) *(180/pi)) 0)) :xs                                                 --  Se a diferença de altura for maior que (0.2) ele troca de 'Pista' e troca o  'Estado' para 'Ar'
trocaPista n b@((Jogador 0 dJ vJ cJ eJ):xs) mapa B = (Jogador 0 dJ vJ cJ eJ) : trocaPista (n-1) xs mapa B                                                                                       --  Fazer recursiva para encontrar o 'Jogador' pretendido
trocaPista  0 (b@(Jogador pJ dJ vJ cJ eJ):xs) mapa C                                                                                                                                            --  'TrocaPista' para movimentar para cima quando a pista não é 0
    | a<(-0.2) = (Jogador pJ dJ 0 cJ (Morto 1.0)) :xs                                                                                                                                           --   Se a diferença de altura for menor que (-0.2) o 'Jogador' morre
    | a<=(0.2) && a>=(-0.2) = (Jogador (pJ-1) dJ vJ cJ eJ) :xs                                                                                                                                  --  Se a diferença de altura for entre (-0.2) e (0.2) ele troca de 'Pista' 
    |otherwise = (Jogador (pJ-1) dJ vJ cJ (Ar (alturaDoJogador (posicaoJogador b mapa) b) ((anguloRampa (posicaoJogador b mapa))*(180/pi)) 0)): xs                                              --  Se a diferença de altura for maior que (0.2) ele troca de 'Pista' e troca o  'Estado' para 'Ar'
        where 
            a = diferencaAltura (Jogador pJ dJ vJ cJ eJ) (posicaoJogador (Jogador pJ dJ vJ cJ eJ) mapa) (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa)
trocaPista n (b@(Jogador pJ dJ vJ cJ eJ):xs) mapa C = (Jogador pJ dJ vJ cJ eJ) : trocaPista (n-1) xs mapa C                                                                                     --  Fazer recursiva para encontrar o 'Jogador' pretendido
trocaPista  0 (b@(Jogador pJ dJ vJ cJ eJ) :xs) mapa B                                                                                                                                           --  'TrocaPista' para movimentar para cima quando a pista não é 0
    | pJ == (length mapa)-1 = (Jogador ((length mapa)-1) dJ vJ cJ eJ :xs)                                                                                                                       --  se o 'Jogador' estiver na última pista ele não baixa de 'Pista'
    | c<(-0.2) = (Jogador pJ dJ 0 cJ (Morto 1.0)) :xs                                                                                                                                           --   Se a diferença de altura for menor que (-0.2) o 'Jogador' morre
    | c<=(0.2) && c>=(-0.2) = (Jogador (pJ+1) dJ vJ cJ eJ)  :xs                                                                                                                                 --  Se a diferença de altura for entre (-0.2) e (0.2) ele troca de 'Pista' 
    |otherwise = (Jogador (pJ+1) dJ vJ cJ (Ar (alturaDoJogador (posicaoJogador b mapa) b) ((anguloRampa (posicaoJogador b mapa))*(180/pi)) 0)) :xs                                              --  Se a diferença de altura for maior que (0.2) ele troca de 'Pista' e troca o  'Estado' para 'Ar'
        where 
            c = diferencaAltura (Jogador pJ dJ vJ cJ eJ) (posicaoJogador (Jogador pJ dJ vJ cJ eJ) mapa) (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa)
trocaPista  n (b@(Jogador pJ dJ vJ cJ eJ) :xs) mapa B  = (Jogador pJ dJ vJ cJ eJ): trocaPista (n-1) xs mapa B                                                                                   --  Fazer recursiva para encontrar o 'Jogador' pretendido

-- | A função 'rodarNoAr' roda 15 graus (positivo ou negativo) quando o Jogador está no Ar 
rodarNoAr :: Int -> [Jogador] -> Direcao -> [Jogador]
rodarNoAr 0 ((Jogador pJ dJ vJ cJ eJ):xs) direcao =
    case eJ of 
        (Ar aJ iJ gJ)
            | iJ < (-90) || iJ > 90 -> (Jogador pJ dJ vJ cJ (Ar aJ (iJ-360) gJ)) : xs
            | iJ <= 90 &&  iJ > 75 && direcao ==E  -> (Jogador pJ dJ vJ cJ (Ar aJ 90 gJ) ) :xs
            | iJ >= (-90) &&  iJ < (-75) && direcao == D ->  (Jogador pJ dJ vJ cJ (Ar aJ (-90) gJ)) :xs 
            | iJ <= 75  && direcao == E ->  (Jogador pJ dJ vJ cJ (Ar aJ (iJ+15) gJ)) :xs
            | iJ >= -75 &&  direcao == D ->  (Jogador pJ dJ vJ cJ (Ar aJ (iJ-15) gJ)):xs
        otherwise -> (Jogador pJ dJ vJ cJ eJ) :xs
rodarNoAr n ((Jogador pJ dJ vJ cJ eJ):xs) direcao = (Jogador pJ dJ vJ cJ eJ) : rodarNoAr (n-1) xs direcao

-- | A função 'usarCola'  faz o Jogador usar cola quando ainda a tiver
usarCola :: Int -> [Jogador] ->  [Jogador] 
usarCola n [] = []
usarCola n ((Jogador pJ dJ vJ cJ eJ):xs) 
        | (n==0) && (cJ ==0) && dJ >= 1 && (eJ == Chao True || eJ == Chao False) = (Jogador pJ dJ vJ 0 eJ) :xs
        | (n == 0) && (cJ /= 0) && dJ >=1 && (eJ == Chao True || eJ == Chao False)= (Jogador pJ dJ vJ (cJ-1) eJ):xs
        | otherwise = (Jogador pJ dJ vJ cJ eJ) : usarCola (n-1) xs

-- | A função 'trocaPeca' Substitui o piso de qualquer Peca para Cola
trocaPeca :: Peca -> Peca 
trocaPeca (Recta piso x ) = Recta Cola x  
trocaPeca (Rampa piso x y) = Rampa Cola x y 

-- | A função 'usarColaMapa' substitui (se possível) a Peca anterior ao Jogador para Cola
usarColaMapa :: Int ->  [Jogador] -> Mapa -> Mapa
usarColaMapa 0 ((Jogador pJ dJ vJ cJ eJ):xs) mapa
    | dJ >= 1 && cJ > 0 && (eJ == Chao True || eJ == Chao False) = atualizaPosicaoMatriz (pJ, (floor (dJ-1))) (trocaPeca (encontraPosicaoMatriz (pJ,floor (dJ-1)) mapa)) mapa
    | otherwise = mapa
usarColaMapa n ((Jogador pJ dJ vJ cJ eJ):xs) mapa = usarColaMapa (n-1) xs mapa


-- | A função 'acelarar' Muda o estado  do Jogador para 'Chao True' se ele inicialmente para   'Chao _' 
acelarar :: Int -> [Jogador] -> [Jogador]
acelarar n [] = []
acelarar  n ((Jogador pJ dJ vJ cJ eJ):xs) 
        | (n == 0) && (eJ == Chao True) = (Jogador pJ dJ vJ cJ (Chao True)) : xs
        | (n == 0) && (eJ == Chao False) = (Jogador pJ dJ vJ cJ (Chao True)) : xs
        | otherwise = (Jogador pJ dJ vJ cJ eJ): acelarar (n-1) xs                                

-- | A função 'desacelera' Muda o estado  do Jogador para 'Chao False' se ele inicialmente para 'Chao _'
desacelera :: Int -> [Jogador] -> [Jogador]
desacelera n [] =[]
desacelera  n ((Jogador pJ dJ vJ cJ eJ):xs) 
        | (n == 0) && (eJ == Chao True) = (Jogador pJ dJ vJ cJ (Chao False)) : xs
        | (n == 0) && (eJ == Chao False) = (Jogador pJ dJ vJ cJ (Chao False)) : xs
        | otherwise = (Jogador pJ dJ vJ cJ eJ): desacelera (n-1) xs 