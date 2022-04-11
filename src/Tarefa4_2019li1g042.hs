{-
O objetivo desta Tarefa era atualizar velocidade e acelaração ao Jogador. Apesar de parecer simples, esta Tarefa mostrou-se mais complicada do que aparentava, apesar de não ser tão difícil como as seguintes.
-}

-- | Este módulo define funções comuns da Tarefa 4 do trabalho prático.
module Tarefa4_2019li1g042 where

import LI11920
import Tarefa2_2019li1g042
import Tarefa0_2019li1g042
import Tarefa1_2019li1g042
-- * Testes
-- | Testes unitários da Tarefa 4.
--
-- Cada teste é um par (/tempo/,/'Mapa'/,/'Jogador'/).
testesT4 :: [(Double,Mapa,Jogador)]
testesT4 =  [(0.1, mapaTeste, j1) , (0.1, mapaTeste, j2), (10, mapaTeste, j1), (10,mapaTeste,j1),(0.1, mapaTeste,j3),(10,mapaTeste,j3),(0.5,mapaTeste,j4), (2,mapaTeste,j4),(0.1,mapaTeste,j5),(2,mapaTeste,j5),(2,mapaTeste,j6), (0.002,mapaTeste,j7),(5,mapaTeste,j7),(5,mapaTeste,j7)]

mapaTeste = [[Recta Terra 0, Rampa Boost 0 1, Rampa Lama 1 3, Rampa Relva 3 2, Recta Terra 2], [Recta Terra 0, Rampa Terra 0 5, Rampa Boost 5 3, Rampa Lama 3 0, Recta Terra 0]]
j1 = Jogador 0 0.2 3 3 (Chao True)
j2 = Jogador 0 0.2 3 3 (Chao False)
j3 = Jogador 0 0.2 3 3 (Ar 2 15 1)
j4 = Jogador 1 3 0 2 (Morto 1.0)
j5 = Jogador 1 1 2 2 (Ar 2 75 2)
j6 = Jogador 1 1 2 2 (Ar 2 0 2)
j7 = Jogador 1 1.3 2 2 (Chao True)
j8 = Jogador 1 1.3 2 2 (Chao False)


-- * Funções principais da Tarefa 4.
-- | Avança o estado de um 'Jogador' um 'passo' em frente, durante um determinado período de tempo.
passo :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após um 'passo'.
passo t m j = move t m (acelera t m j)


-- | Altera a velocidade de um 'Jogador', durante um determinado período de tempo.
acelera :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após acelerar.
acelera t m j@(Jogador pJ dJ vJ cJ (Chao x)) = (Jogador pJ dJ (ifNegativo (aceleraChao t m j)) cJ (Chao x)) 
acelera t m j@(Jogador pJ dJ vJ cJ (Ar a i g)) = (Jogador pJ dJ (ifNegativo (aceleraAr t m j)) cJ (Ar a i (novaGravidade t j))) 
acelera t m j@(Jogador pJ dJ vJ cJ (Morto x)) = (Jogador pJ dJ 0 cJ (Morto x))


-- | Altera a posição de 'Jogador', durante um determinado período de tempo.
move :: Double -- ^ O tempo decorrido.
     -> Mapa    -- ^ O mapa utilizado.
     -> Jogador -- ^ O estado anterior do 'Jogador'.
     -> Jogador -- ^ O estado do 'Jogador' após se movimentar.
move t mapa j@(Jogador pJ dJ vJ cJ (Morto x)) =  movimentaMorto t j
move t mapa j@(Jogador pJ dJ vJ cJ (Chao x))
     | dJ >= (fromIntegral (length (head mapa)) -1) = Jogador pJ (dJ + ((vJ* cos iJ)*t)) vJ cJ (Chao x)
     | vJ* (cos iJ) *t < 1- (dJ - (fromIntegral (floor dJ))) = movimentaChaoChao t mapa j
     | anguloRampa (posicaoJogador j mapa) <= anguloRampa (posicaoJogador (Jogador pJ (dJ+1) vJ cJ (Chao x)) mapa) = movimentaChaoChao t mapa j
     | otherwise = movimentaChaoAr t  mapa j
          where
               iJ = anguloRampa (posicaoJogador j mapa)
move t mapa j@(Jogador pJ dJ vJ cJ (Ar a i g))
     | intersecaoChaoAr t (posicaoJogador j mapa) j =  movimentaArChao t mapa j
     | otherwise = movimentaArAr t mapa j

-- | A função  'aceleraChao'  aplica acelaração para um 'Jogador'
aceleraChao :: Double -> Mapa -> Jogador -> Double
aceleraChao t m j@(Jogador pJ dJ vJ cJ eJ)  = vJ + ((accelMota j - (atrito m j) * vJ) * t)  

-- | A função 'atrito' define o atrito para os diferentes pisos
atrito :: Mapa -> Jogador -> Double
atrito m j | piso (posicaoJogador j m) == Terra = 0.25
    	      | piso (posicaoJogador j m) == Relva = 0.75
           | piso (posicaoJogador j m) == Lama  = 1.50
           | piso (posicaoJogador j m) == Boost = -0.50
 	      | otherwise = 3.00

-- | A função 'piso' diz qual é o piso de uma 'Recta' ou 'Rampa'
piso :: Peca -> Piso 
piso (Recta p a) = p
piso (Rampa p ai af) = p  

-- | A função 'accelMota' troca o valor da acelaração para 0 ou 1 dependendo do estado do 'Jogador'
accelMota :: Jogador -> Double 
accelMota j@(Jogador pJ dJ vJ cJ eJ) | (vJ<2 &&  accelJogador j)  = 1
		                             | otherwise = 0 

-- | A função 'accelJogador' verifica se o estado do 'Jogador' é '(Chao True)'
accelJogador :: Jogador -> Bool 
accelJogador (Jogador pJ dJ vJ cJ (Chao x)) = x
accelJogador _ = False 

-- | A função 'aceleraAr' aplica acelaração para quando o 'Jogador' está no ar 
aceleraAr :: Double -> Mapa -> Jogador -> Double
aceleraAr t m j@(Jogador pJ dJ vJ cJ eJ) = vJ - (resistenciaAr * vJ * t)
          where 
               resistenciaAr = 0.125

-- | A função 'novaGravidade' atualiza a gravidade para quando o 'Jogador' está no ar
novaGravidade :: Double -> Jogador -> Double
novaGravidade t j@(Jogador pJ dJ vJ cJ (Ar a i g)) = g + accelGravidade * t
        where 
          accelGravidade = 1.0

-- | A função 'ifNegativo' troca a velocidade para 0 caso ela passe para negativa
ifNegativo :: Double -> Double
ifNegativo x | x<0 = 0 
             | otherwise = x 

-- | A função 'movimentaMorto' verifica se um 'Jogador' morto, passado um certo tempo, passa para 'Chao False' ou continua morto, com menor tempo de ressureição 
movimentaMorto :: Double -> Jogador -> Jogador
movimentaMorto t (Jogador pJ dJ vJ cJ (Morto x))
          | (x-t) <= 0 = Jogador pJ dJ vJ cJ (Chao False)
          | otherwise = Jogador pJ dJ vJ cJ (Morto (x-t))


-- | A função 'movimentaChaoChao' vê ,quando ele está no chão e continua no chao, o que se vai passar passado um certo tempo (trocar de 'Peca' ou andar uma certa distãncia na mesma 'Peca')
movimentaChaoChao :: Double -> Mapa -> Jogador -> Jogador 
movimentaChaoChao t mapa j@(Jogador pJ dJ vJ cJ (Chao x))
          | (vJ* (cos iJ) *t) <= 1- (dJ - (fromIntegral (floor dJ))) = Jogador pJ (dJ + ((vJ* cos iJ)*t)) vJ cJ (Chao x)
          | otherwise  = Jogador pJ (fromIntegral (floor(dJ+1)))  vJ cJ (Chao x)
          where
               iJ = anguloRampa (posicaoJogador j mapa)

-- | A função 'movimentaChaoChao' vê ,quando ele está no chão e vai possivelmente para o ar, o que vai se passar passado um certo tempo (trocar de 'Peca', passando para o ar e alterando o seu estado, ou andar uma certa distãncia na mesma 'Peca', não mudando o seu estado)
movimentaChaoAr :: Double -> Mapa -> Jogador -> Jogador 
movimentaChaoAr t mapa j@(Jogador pJ dJ vJ cJ (Chao z)) 
          | (vJ*(cos iJ) *t) < 1- (dJ - (fromIntegral (floor dJ))) = Jogador pJ  (dJ + ((vJ* cos iJ)*t))  vJ cJ (Chao z)
          | otherwise = Jogador pJ (fromIntegral (floor (dJ+1))) vJ cJ (Ar (alturaFinalPeca peca) (iJ * 180/pi) 0) 
          where
               iJ = anguloRampa peca
               peca = posicaoJogador j mapa

-- | A função 'alturaFinalPeca' diz a altura final de uma 'Peca' em forma de 'Double'
alturaFinalPeca :: Peca -> Double
alturaFinalPeca (Rampa piso y x) = fromIntegral x
alturaFinalPeca (Recta piso x)   = fromIntegral x

-- | A função 'movimentaArAr' vê ,quando ele está no ar e continua no ar, o que se vai passar passado um certo tempo (trocar de 'Peca' ou andar uma certa distãncia na mesma 'Peca', alterando, em ambos os casos, a sua altura)
movimentaArAr :: Double -> Mapa -> Jogador -> Jogador
movimentaArAr t mapa j@(Jogador pJ dJ vJ cJ (Ar aJ iJ gJ))         
          | ((vJ*cos ((iJ*pi)/180)) *t) < 1- (dJ - (fromIntegral (floor dJ))) = Jogador pJ (dJ + ((vJ* cos ((iJ*pi)/180))*t))  vJ cJ (Ar (aJ- (gJ*t) + (vJ *sin ((iJ*pi)/180))*t) iJ gJ)
          | otherwise = Jogador pJ (fromIntegral (floor (dJ+1)))  vJ cJ (Ar (pontoParaAltura (pontoIntersecaoAr t j (posicaoJogador j mapa))) iJ gJ)

-- | A função 'movimentaArChao' vê ,quando ele está no chão e passa possivelmente para o ar, o que se vai passar passado um certo tempo (trocar de 'Peca' e ir para o ar, atualizando o seu estado, ou andar uma certa distãncia na mesma 'Peca', continuando no Chao)
movimentaArChao :: Double -> Mapa -> Jogador -> Jogador
movimentaArChao t mapa j@(Jogador pJ dJ vJ cJ (Ar aJ iJ gJ))
          | (iJ+45 <= anguloRampa (posicaoJogador j mapa)) ||  (iJ-45 >= anguloRampa (posicaoJogador j mapa)) = pontoParaDistanciaMorto j (pontoIntersecao t j (posicaoJogador j mapa))
          | otherwise = (Jogador  pJ (pontoParaDistancia j (pontoIntersecao t j (posicaoJogador j mapa))) (vJ* cos (((iJ - anguloRampaGraus (posicaoJogador j mapa))*pi)/180)) cJ (Chao False))

-- | A função 'anguloRampaGraus' calcula a inclinação de uma 'Peca', dando o seu valor em graus
anguloRampaGraus :: Peca -> Double
anguloRampaGraus x = ((anguloRampa x) * 180)/pi

-- | A função 'intersecaoChaoAr' utiliza a função intersetam da Tarefa 0 e verifica se o 'Jogador', começando no Ar, interseta o Chão, num determinado tempo 
intersecaoChaoAr :: Double -> Peca -> Jogador -> Bool
intersecaoChaoAr t (Recta tipo x) (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ))   = intersetam (Cartesiano dJ aJ, Cartesiano (dJ + ((vJ* cos ((iJ*pi)/180))*t)) (aJ- (gJ*t) + (vJ *sin ((iJ*pi)/180)) *t)) (Cartesiano (fromIntegral (floor dJ)) (fromIntegral x), Cartesiano (fromIntegral (floor (dJ+1))) (fromIntegral x))
intersecaoChaoAr t (Rampa tipo x y) (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) = intersetam (Cartesiano dJ aJ, Cartesiano (dJ + ((vJ* cos ((iJ*pi)/180))*t)) (aJ- (gJ*t) + (vJ *sin ((iJ*pi)/180)) *t)) (Cartesiano (fromIntegral (floor dJ)) (fromIntegral x), Cartesiano (fromIntegral (floor (dJ+1))) (fromIntegral y))

-- | A função 'alturaPeca' dá a altura final de uma 'Peca', em forma de um 'Int'
alturaPeca :: Peca -> Int
alturaPeca (Recta tipo x)   = x
alturaPeca (Rampa tipo x y) = y

-- | Caso a função 'intersetaChaoAr' dê True, a função 'pontoIntersecao'  calcula o ponto de interseção onde o 'Jogador' interseta o Chão
pontoIntersecao :: Double -> Jogador -> Peca -> Ponto
pontoIntersecao t (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) (Recta tipo x)   = intersecao  (Cartesiano dJ aJ, Cartesiano (dJ + ((vJ* cos ((iJ*pi)/180))*t)) (aJ- (gJ*t) + (vJ *sin ((iJ*pi)/180)) *t)) (Cartesiano (fromIntegral (floor dJ)) (fromIntegral x), Cartesiano (fromIntegral (floor (dJ+1))) (fromIntegral x))
pontoIntersecao t (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) (Rampa tipo x y) = intersecao  (Cartesiano dJ aJ, Cartesiano (dJ + ((vJ* cos ((iJ*pi)/180))*t)) (aJ- (gJ*t) + (vJ *sin ((iJ*pi)/180)) *t)) (Cartesiano (fromIntegral (floor dJ)) (fromIntegral x), Cartesiano (fromIntegral (floor (dJ+1))) (fromIntegral y))

-- | Caso a função 'intersetaChaoAr' dê False, a função 'pontoIntersecaoAr'  calcula o ponto de interseção onde o 'Jogador' troca de 'Peca'
pontoIntersecaoAr :: Double -> Jogador -> Peca -> Ponto
pontoIntersecaoAr t (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) (Recta tipo x)   = intersecao  (Cartesiano dJ aJ, Cartesiano (dJ + ((vJ* cos ((iJ*pi)/180))*t)) (aJ- (gJ*t) + (vJ *sin ((iJ*pi)/180)) *t)) (Cartesiano (fromIntegral (floor (dJ+1))) (fromIntegral x), Cartesiano (fromIntegral (floor (dJ+1))) (10000))
pontoIntersecaoAr t (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) (Rampa tipo x y) = intersecao  (Cartesiano dJ aJ, Cartesiano (dJ + ((vJ* cos ((iJ*pi)/180))*t)) (aJ- (gJ*t) + (vJ *sin ((iJ*pi)/180)) *t)) (Cartesiano (fromIntegral (floor (dJ+1))) (fromIntegral x), Cartesiano (fromIntegral (floor (dJ+1))) (10000))

-- | A função 'pontoParaAltura' dá a segunda componente do Cartesiano que ,neste caso, vai corrresponder à altura 
pontoParaAltura :: Ponto -> Double
pontoParaAltura (Cartesiano x y) = y

-- | A função 'pontoParaDistancia' dá a primeira componente do Cartesiano que ,neste caso, vai corrresponder à distância 
pontoParaDistancia :: Jogador -> Ponto -> Double 
pontoParaDistancia (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) (Cartesiano x y) = x

-- | A função 'pontoParaDistanciaMorto'  devolve um 'Jogador' morto (vai ser usada quando a diferença de inclinação entre o 'Jogador' que está no ar e o Chão for maior que 45º)
pontoParaDistanciaMorto :: Jogador -> Ponto -> Jogador
pontoParaDistanciaMorto (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) (Cartesiano x y) = Jogador pJ x 0 cJ (Morto 1.0)