{- |
= Introdução

O objetivo desta tarefa é a criação de um bot, ou seja, um jogador capaz de jogar sem nenhum comando ordenado pelo humano. Esta tarefa desafiou-nos a pensar em todos casos
possíveis para melhorar ao máximo o comportamento do nosso bot.

= Objetivos

O nosso objetivo foi maximizar o desempenho do nosso bot. Para cumprir esse objetivo , tentamos definir novas funções para o bot saber o que fazer em todos os casos possíveis. 
Contudo, como o número de casos possiveis é enorme, é quase impossível fazer um bot que tenha o melhor desempenho em todos os casos. Posto isto, definimos casos para um certo 
número de acontecimentos, por exemplo:

* Caso todas as pistas da próxima coluna tiverem a mesma altura ou diferença da altura, o bot analisa e escolhe a 'Peca' com menor atrito
* Caso contrário, o bot analisa e vê qual a opcão mais rentavel pela Rampa ou pelas outras pistas
* Se ele tiver no ar, vai tentar aterrar no chao com diferença de menos de 45º e se ele estiver afastado o suficiente, vai inclinar-se o máximo para (-90º) para cair o mais rápido possível
* O nosso bot, por definição, está sempre a acelerar
* Se ele estiver numa peça Boost, dispara Cola

= Discussão e conclusão

O nosso bot não está totalmente otimizado para quando há rampas à sua frente e também quando está no ar. No entanto, achamos que o desempenho do bot é satisfatório embora não
tenhamos conseguido por o nosso bot finalizado a competir em nenhum torneio. 
Em conclusão, esta tarefa foi uma das mais complicadas e trabalhosas, visto que, foi um desafio enorme pensar em casos para melhorar o bot e por esses pensamentos em linguagem
Haskell.
-}

-- | Este módulo define funções comuns da Tarefa 6 do trabalho prático.
module Tarefa6_2019li1g042 where

import LI11920
import Tarefa4_2019li1g042
import Tarefa1_2019li1g042
import Tarefa2_2019li1g042

-- * Funções principais da Tarefa 6.

mapa61 = [[Recta Terra 0, Recta Lama 0, Rampa Boost 0 1, Recta Terra 1, Rampa Cola 1 0],[Recta Terra 0, Rampa Terra 0 1, Rampa Terra 0 1, Rampa Terra 0 2, Recta Terra 2], [Recta Terra 0, Rampa Terra 0 1, Rampa Terra 0 1, Recta Terra 0], [Recta Terra 0, Recta Terra 0, Recta Terra 0, Recta Terra 0]]
mapa62 = [[Recta Terra 0, Rampa Boost 0 1, Recta Boost 1, Recta Boost 1, Recta Boost 1], [Recta Terra 0, Recta Terra 0, Recta Terra 0, Recta Terra 0,Recta Terra 0] , [Recta Terra 0,Recta Terra 0,Recta Terra 0 ,Rampa Boost 0 1,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0]]

-- | Define um ro'bot' capaz de jogar autonomamente o jogo.
bot :: Int          -- ^ O identificador do 'Jogador' associado ao ro'bot'.
    -> Estado       -- ^ O 'Estado' para o qual o ro'bot' deve tomar uma decisão.
    -> Maybe Jogada -- ^ Uma possível 'Jogada' a efetuar pelo ro'bot'.
bot n (Estado mapa ((Jogador pJ dJ vJ cJ (Chao False)):js))= Just Acelera
bot n (Estado mapa (j@(Jogador pJ dJ vJ cJ (Chao True)):js))
    | disparaCola mapa j = Just Dispara 
    | alturaDasPistasIgual mapa (Jogador pJ dJ vJ cJ (Chao True)) = pistasPrioritariasRetas j (primeiraColuna (dropxColunas (floor dJ) mapa))
    | otherwise = jogadaRampa mapa j
bot n (Estado mapa (j@(Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) :js)) 
    | intersecaoChaoAr 1 (posicaoJogador j mapa) j = arIntersecao mapa j
    | otherwise = arSemIntersecao j

-- | A função 'dropxColunas' dropa um x número de colunas num mapa
dropxColunas :: Int -> Mapa -> Mapa
dropxColunas x mapa =  map (drop x) mapa

-- | A função 'primeiraColuna' pega numa coluna de um 'Mapa'
primeiraColuna :: Mapa -> [Peca]
primeiraColuna [] = []
primeiraColuna (x:xs) = map (head ) (x:xs)

-- | A função 'atritoPecaReta' dá o atrito de cada 'Peca', caso seja uma reta 
atritoPecaReta :: Peca -> Double
atritoPecaReta peca 
    | piso peca == Terra = 0.25
    | piso peca == Relva = 0.75
    | piso peca == Lama  = 1.5
    | piso peca == Boost = -0.5
    | otherwise = 3.00

-- | A função 'atritoPeca' dá o atrito de uma 'Peca' , tendo em conta se é Rampa (não é assim que ela é aplicada na Tarefa 4, é só para ter ideia das prioridades)
atritoPeca :: Peca -> Double
atritoPeca (Recta tipo x) = atritoPecaReta (Recta tipo x)
atritoPeca a@(Rampa tipo x y) = (atritoPecaReta a)/ (cos (anguloRampa a))

-- | A função 'prioridadeColuna' vê numa coluna qual é a peca mais vantajosa para ir (a que tem menos atrito)
prioridadeColuna :: [Peca]  -> Peca
prioridadeColuna (x:[]) = x
prioridadeColuna (x:y:xs) 
    | atritoPeca x < atritoPeca y = prioridadeColuna (x:xs)
    | otherwise = prioridadeColuna (y:xs)


-- | A função 'elemIndices' diz em que posição um certo elemento está numa lista
elemIndices ::  Eq a => a ->[a] -> [Int]
elemIndices n [] = []
elemIndices n (x:xs) 
    | n == x = 0 : map  (+1) (elemIndices n xs)
    | otherwise = map (+1) (elemIndices n xs)

-- | A função 'int' transforma uma '[Int]' para 'Int'
int :: [Int] -> Int
int [x] =x
int (x:xs) =x

-- | A função 'pistaPrioridade' diz em que pista está a 'Peca' mais rentável (menor atrito)
pistaPrioridade :: [Peca] -> Int
pistaPrioridade (x:xs) = int ((elemIndices (prioridadeColuna (x:xs)) (x:xs)))

-- | A função 'pistasPrioritariasRetas' dirá o movimento que o 'Jogador' fará, se forem só retas na próxima coluna do 'Mapa'.
pistasPrioritariasRetas :: Jogador -> [Peca] -> Maybe Jogada
pistasPrioritariasRetas j@(Jogador pJ dJ vJ cJ eJ) (y:ys)
    | pJ == 0 || pJ == 3 = naoMorrerReta j (y:ys)
    | (alturaDoJogador (head (drop pJ (y:ys))) j - alturaDoJogador (head (drop (pJ-1) (y:ys))) j /= 0) || (alturaDoJogador (head (drop pJ (y:ys))) j - alturaDoJogador (head (drop (pJ+1) (y:ys))) j /= 0) = naoMorrerReta j (y:ys)
    | pJ < pistaPrioridade (y:ys) = Just (Movimenta B)
    | pJ > pistaPrioridade (y:ys) = Just (Movimenta C)
    | otherwise = Nothing

-- | A função 'naoMorrerReta' dirá o movimento que o 'Jogador' fará, tendo em conta Rampas e Retas de diferente elevação que hajam na coluna em que o 'Jogador' está, ou se ele está na primeira ou última 'Pista'
naoMorrerReta :: Jogador -> [Peca] -> Maybe Jogada
naoMorrerReta j@(Jogador pJ dJ vJ cJ eJ) (y:ys)
    | pJ == 0 = if alturaDoJogador y j - alturaDoJogador (head(drop 1 (y:ys))) j /= 0 then Nothing
                    else if pJ /= pistaPrioridade (y:ys)  then Just (Movimenta B)
                        else Nothing 
    | pJ == 3 = if alturaDoJogador (last (y:ys)) j - alturaDoJogador (head(drop 2 (y:ys))) j /= 0 then Nothing
                    else if pJ /= pistaPrioridade (y:ys) then Just (Movimenta C)
                        else Nothing
    | (alturaDoJogador (head (drop pJ (y:ys))) j - alturaDoJogador (head (drop (pJ-1) (y:ys))) j /= 0) && (alturaDoJogador (head (drop pJ (y:ys))) j - alturaDoJogador (head (drop (pJ+1) (y:ys))) j /= 0) = Nothing
    | (alturaDoJogador (head (drop pJ (y:ys))) j - alturaDoJogador (head (drop (pJ-1) (y:ys))) j == 0) && (alturaDoJogador (head (drop pJ (y:ys))) j - alturaDoJogador (head (drop (pJ+1) (y:ys))) j /= 0) =  if  pJ > pistaPrioridade (take(pJ+1) (y:ys)) then Just (Movimenta C)
                                                                                                                                                                                                                    else Nothing
    | (alturaDoJogador (head (drop pJ (y:ys))) j - alturaDoJogador (head (drop (pJ-1) (y:ys))) j /= 0) && (alturaDoJogador (head (drop pJ (y:ys))) j - alturaDoJogador (head (drop (pJ+1) (y:ys))) j == 0) = if 0 < pistaPrioridade (drop pJ (y:ys)) then Just (Movimenta B)
                                                                                                                                                                                                                    else Nothing

-- | A função 'alturaDasPistasIgual'  verifica, num 'Mapa' com quatro pistas, se a altura final das peças da coluna seguinte em relação ao 'Jogador' é igual 
alturaDasPistasIgual :: Mapa -> Jogador -> Bool
alturaDasPistasIgual mapa j@(Jogador pJ dJ vJ cJ eJ)
    | dJ == fromIntegral (length (head mapa)) -1 = True
    | dJ == fromIntegral (length (head mapa)) -2 = True
    | (alturaDoJogador (posicaoJogador (Jogador 0 (fromIntegral (floor (dJ+2))) vJ cJ eJ) mapa) (Jogador 0 (fromIntegral (floor (dJ+2))) vJ cJ eJ)) - (alturaDoJogador (posicaoJogador ( Jogador 1 (fromIntegral (floor (dJ+2))) vJ cJ eJ) mapa) ( Jogador 1 (fromIntegral (floor (dJ+2))) vJ cJ eJ)) == 0 = True
    | (alturaDoJogador (posicaoJogador (Jogador 1 (fromIntegral (floor (dJ+2))) vJ cJ eJ) mapa) (Jogador 1 (fromIntegral (floor (dJ+2))) vJ cJ eJ)) - (alturaDoJogador (posicaoJogador ( Jogador 2 (fromIntegral (floor (dJ+2))) vJ cJ eJ) mapa) ( Jogador 2 (fromIntegral (floor (dJ+2))) vJ cJ eJ)) == 0 = True
    | (alturaDoJogador (posicaoJogador (Jogador 2 (fromIntegral (floor (dJ+2))) vJ cJ eJ) mapa) (Jogador 2 (fromIntegral (floor (dJ+2))) vJ cJ eJ)) - (alturaDoJogador (posicaoJogador ( Jogador 3 (fromIntegral (floor (dJ+2))) vJ cJ eJ) mapa) ( Jogador 3 (fromIntegral (floor (dJ+2))) vJ cJ eJ)) == 0 = True
    | otherwise = False

-- | Caso a 'alturaDasPistasIgual' for False, a função 'pistaDaDiferençaAltura' diz qual é a 'Pista' onde ha diferença de altura
pistaDaDiferencaAltura :: Mapa -> Jogador -> (Int,Double)
pistaDaDiferencaAltura mapa j@(Jogador pJ dJ vJ cJ eJ)
    | anguloRampa (posicaoJogador (Jogador pJ (dJ+1) vJ cJ eJ) mapa) /= 0 = (pJ , dJ+1)
    | otherwise = pistaDaDiferencaAltura  mapa(Jogador (pJ+1) dJ vJ cJ eJ)

-- | A função 'alturaInicial' diz a altura inicial de uma 'Peça'
alturaInicial :: Peca -> Double
alturaInicial (Recta tipo x)   = fromIntegral x
alturaInicial (Rampa tipo x y) = fromIntegral x

-- | A função 'quantasPecasSozinhoCima' vê quantas peças a pista com diferenca de elevacao esta sem contacto com a pista de cima, ou seja, sem terem a mesma altura num mesmo ponto
quantasPecasSozinhoCima :: Int -> Mapa -> Jogador -> Int
quantasPecasSozinhoCima x mapa j@(Jogador pJ dJ vJ cJ eJ) 
    | dJ +1 >= fromIntegral (length (head mapa)) = x 
    | (alturaInicial (posicaoJogador j mapa) >= alturaInicial (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa)) && (alturaFinalPeca (posicaoJogador j mapa) > alturaFinalPeca (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa)) = quantasPecasSozinhoCima (x+1) mapa (Jogador pJ (dJ-1) vJ cJ eJ)
    | (alturaInicial (posicaoJogador j mapa) <= alturaInicial (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa)) && (alturaFinalPeca (posicaoJogador j mapa) < alturaFinalPeca (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa)) = quantasPecasSozinhoCima (x+1) mapa (Jogador pJ (dJ-1) vJ cJ eJ)
    | otherwise = x

-- | A função 'quantasPecasSozinhoBaixo' vê quantas peças a pista com diferenca de elevacao esta sem contacto com a pista de baixo, ou seja, sem terem a mesma altura num mesmo ponto
quantasPecasSozinhoBaixo :: Int -> Mapa -> Jogador -> Int
quantasPecasSozinhoBaixo x mapa j@(Jogador pJ dJ vJ cJ eJ)
    | dJ +1 >= fromIntegral (length (head mapa)) = x +1 
    | (alturaInicial (posicaoJogador j mapa) >= alturaInicial (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa)) && (alturaFinalPeca (posicaoJogador j mapa) > alturaFinalPeca (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa)) = quantasPecasSozinhoBaixo (x+1) mapa (Jogador pJ (dJ+1) vJ cJ eJ)
    | (alturaInicial (posicaoJogador j mapa) <= alturaInicial (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa)) && (alturaFinalPeca (posicaoJogador j mapa) < alturaFinalPeca (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa)) = quantasPecasSozinhoBaixo (x+1) mapa (Jogador pJ (dJ+1) vJ cJ eJ)
    | otherwise = x +1

-- | A função 'quantasPecasSozinho' vê quantas peças a pista com diferença de elevação está sem contacto com a pista de baixo e decima e diz qual é a maior distância
quantasPecasSozinho :: Int -> Mapa -> Jogador -> Int
quantasPecasSozinho x mapa j@(Jogador 0 dJ vJ cJ eJ) = quantasPecasSozinhoBaixo x mapa j 
quantasPecasSozinho x mapa j@(Jogador pJ dJ vJ cJ eJ) 
    | pJ == (length mapa) -1 = quantasPecasSozinhoCima x mapa j
    | otherwise =  maximum ((quantasPecasSozinhoBaixo x mapa j) , quantasPecasSozinhoCima x mapa j) 

-- | A função 'pistasSozinhaCompleta' dá a 'Pista' da rampa identificada acima, desde a 'dJ' até ao fim da 'Pista'
pistaSozinhaCompleta :: Mapa -> Jogador -> Pista
pistaSozinhaCompleta mapa j@(Jogador pJ dJ vJ cJ eJ) 
    | fst (pistaDaDiferencaAltura mapa j) == 0 =  (drop (fromInteger (floor dJ)) (head mapa))
    | fst (pistaDaDiferencaAltura mapa j) == 1 =  (drop (fromInteger (floor dJ)) (head (drop 1 mapa)))
    | fst (pistaDaDiferencaAltura mapa j) == 2 =  (drop (fromInteger (floor dJ)) (head (drop 2 mapa)))
    | otherwise  = (drop (fromInteger (floor dJ)) (head (drop 3 mapa)))

-- | A função 'pistaSozinho' dá a 'Pista' da rampa identificada acima, desde a 'dJ' até onde há contacto com outra 'Pista'
pistaSozinho :: Mapa -> Jogador -> Pista
pistaSozinho mapa j = take (quantasPecasSozinho 0 mapa j) (pistaSozinhaCompleta mapa j)

-- | A função 'atritoTotalDaPista' soma os atritosde todas as peças de uma 'Pista'
atritoTotalDaPista :: Pista -> Double
atritoTotalDaPista [] = 0
atritoTotalDaPista (x:xs) = atritoPeca x + atritoTotalDaPista xs


-- | A função 'melhorDistancia1PistaFora' decide se é melhor ir pela rampa ou não, calculando o total de atrito desde o início da rampa até que haja contacto novamente e o total de atrito num percurso mais ou menos eficiente desde a coluna onde está a rampa até à coluna do ponto de contacto
melhorDistancia1PistaFora :: Mapa -> Jogador -> Pista
melhorDistancia1PistaFora mapa j@(Jogador pJ dJ vJ cJ eJ) 
   | alturaDasPistasIgual (drop 1 mapa) (Jogador 0 dJ vJ cJ eJ) =  if (atritoTotalDaPista (pistaSozinho mapa j)) < atritoTotalDaPista (pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (tail mapa))))
                                                                        then pistaSozinho mapa j
                                                                        else pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (tail mapa)))

-- | A função 'melhorDistancia2PistaFora' decide se é melhor ir pela rampa ou não, calculando o total de atrito desde o início da rampa até que haja contacto novamente e o total de atrito num percurso mais ou menos eficiente desde a coluna onde está a rampa até à coluna do ponto de contacto
melhorDistancia2PistaFora :: Mapa -> Jogador -> Pista
melhorDistancia2PistaFora mapa j@(Jogador pJ dJ vJ cJ eJ)
    | (atritoTotalDaPista  (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (head mapa))) < (atritoTotalDaPista (pistaSozinho mapa j))) &&  (atritoTotalDaPista  (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (head mapa))) < atritoTotalDaPista (pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (drop 2 mapa))))) = (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (head mapa)))
    | (atritoTotalDaPista (pistaSozinho mapa j)) < (atritoTotalDaPista  (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (head mapa)))) &&  (atritoTotalDaPista (pistaSozinho mapa j)) < atritoTotalDaPista (pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (drop 2 mapa)))) = pistaSozinho mapa j
    | otherwise = pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (drop 2 mapa)))

-- | A função 'melhorDistancia3PistaFora' decide se é melhor ir pela rampa ou não, calculando o total de atrito desde o início da rampa até que haja contacto novamente e o total de atrito num percurso mais ou menos eficiente desde a coluna onde está a rampa até à coluna do ponto de contacto
melhorDistancia3PistaFora :: Mapa -> Jogador -> Pista
melhorDistancia3PistaFora mapa j@(Jogador pJ dJ vJ cJ eJ)
    | (atritoTotalDaPista  (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (last mapa))) < (atritoTotalDaPista (pistaSozinho mapa j))) &&  (atritoTotalDaPista  (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (last mapa))) < atritoTotalDaPista (pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (take 2 mapa))))) = (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (last mapa)))
    | (atritoTotalDaPista (pistaSozinho mapa j)) < (atritoTotalDaPista  (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (last mapa)))) &&  (atritoTotalDaPista (pistaSozinho mapa j)) < atritoTotalDaPista (pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (take 2 mapa)))) = pistaSozinho mapa j
    | otherwise = pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (take 2 mapa)))

-- | A função 'melhorDistancia4PistaFora' decide se é melhor ir pela rampa ou não, calculando o total de atrito desde o início da rampa até que haja contacto novamente e o total de atrito num percurso mais ou menos eficiente desde a coluna onde está a rampa até à coluna do ponto de contacto
melhorDistancia4PistaFora :: Mapa -> Jogador -> Pista
melhorDistancia4PistaFora mapa j@(Jogador pJ dJ vJ cJ eJ)
    | (atritoTotalDaPista (pistaSozinho mapa j)) < atritoTotalDaPista (pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (take 3 mapa)))) = pistaSozinho mapa j
    | otherwise = pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (take 3 mapa)))

-- | A função 'pecasRentaveis' cria uma pista com as pecas mais eficientes (1 em cada coluna) excluindo a pista onde está a rampa
pecasRentaveis :: Mapa -> Pista
pecasRentaveis [] = []
pecasRentaveis (x:xs) 
    | length x == 1 = [prioridadeColuna (primeiraColuna (x:xs))]
    | otherwise = prioridadeColuna (primeiraColuna (x:xs)) : pecasRentaveis ((map (tail)) (x:xs))

-- | A função 'quePistaEscolher' selecao a pista mais eficiente , tendo em conta que uma delas ,pelo menos, tem uma rampa
quePistaEscolher :: Mapa -> Jogador -> Pista
quePistaEscolher mapa j
    | fst (pistaDaDiferencaAltura mapa j) == 0 = melhorDistancia1PistaFora mapa j
    | fst (pistaDaDiferencaAltura mapa j) == 1 = melhorDistancia2PistaFora mapa j
    | fst (pistaDaDiferencaAltura mapa j) == 2 = melhorDistancia3PistaFora mapa j
    | otherwise = melhorDistancia4PistaFora mapa j

-- | A função 'pistaMaisRentavelRampa' vê qual é a 'Pista' mais eficiente paar o bot ir
pistaMaisRentavelRampa :: Mapa -> Jogador -> Int
pistaMaisRentavelRampa mapa j@(Jogador pJ dJ vJ cJ eJ) 
    | quePistaEscolher mapa j == pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (tail mapa))) = 2
    | quePistaEscolher mapa j == (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (head mapa))) = 0 
    | quePistaEscolher mapa j == pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (drop 2 mapa))) = 3
    | quePistaEscolher mapa j == (take (quantasPecasSozinhoCima 0 mapa j) (drop (fromIntegral (floor dJ)) (last mapa))) = 3
    | quePistaEscolher mapa j == pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (take 2 mapa))) = 1
    | quePistaEscolher mapa j == pecasRentaveis (take (quantasPecasSozinho 0 mapa j) (drop (fromIntegral (floor dJ)) (take 3 mapa))) = 0
    | otherwise = if pistaSozinho mapa j == melhorDistancia1PistaFora mapa j then 0
                    else if pistaSozinho mapa j == melhorDistancia2PistaFora mapa j then 1
                        else if pistaSozinho mapa j == melhorDistancia3PistaFora mapa j then 2
                            else 3

-- | A função 'jogadaRampa' vê qual a próxima 'Jogada' que o bot vai fazer
jogadaRampa ::Mapa -> Jogador  -> Maybe Jogada
jogadaRampa mapa j@(Jogador pJ dJ vJ cJ eJ) 
    | pJ== 0  || pJ == 3 = naoMorrer mapa j
    | (alturaDoJogador (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa) (Jogador (pJ+1) dJ vJ cJ eJ)) - (alturaDoJogador (posicaoJogador j mapa) j) /= 0 || (alturaDoJogador (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa) (Jogador (pJ-1) dJ vJ cJ eJ)) - (alturaDoJogador (posicaoJogador j mapa) j) /= 0 = naoMorrer mapa j
    | pJ == fst (pistaDaDiferencaAltura mapa (Jogador pJ (dJ -1) vJ cJ eJ)) = Nothing
    | pJ < pistaMaisRentavelRampa mapa j = Just (Movimenta C)
    | pJ > pistaMaisRentavelRampa mapa j = Just (Movimenta B)
    | otherwise  = Nothing 

-- | A função 'naoMorrer' vê a próxima Jogada, sabendo que o 'Jogador' está na primeira ou última 'Pista' ou se tem uma Rampa em algumas das pistas adjacentes 
naoMorrer :: Mapa -> Jogador -> Maybe Jogada
naoMorrer mapa j@(Jogador pJ dJ vJ cJ eJ)
    | pJ == 0 = pistasPrioritariasRetas j [(posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa) , (posicaoJogador j mapa) ]
    | pJ == 3 =  pistasPrioritariasRetas j [(posicaoJogador j mapa),(posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa) ]
    | (alturaDoJogador (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa) (Jogador (pJ+1) dJ vJ cJ eJ) - alturaDoJogador (posicaoJogador j mapa) j /= 0) && (alturaDoJogador (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa) (Jogador (pJ-1) dJ vJ cJ eJ) - alturaDoJogador (posicaoJogador j mapa) j /= 0) = Nothing
    | (alturaDoJogador (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa) (Jogador (pJ+1) dJ vJ cJ eJ) - alturaDoJogador (posicaoJogador j mapa) j /= 0) && (alturaDoJogador (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa) (Jogador (pJ-1) dJ vJ cJ eJ) - alturaDoJogador (posicaoJogador j mapa) j == 0) = pistasPrioritariasRetas j [(posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa) , (posicaoJogador j mapa) ]
    | (alturaDoJogador (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa) (Jogador (pJ+1) dJ vJ cJ eJ) - alturaDoJogador (posicaoJogador j mapa) j == 0) && (alturaDoJogador (posicaoJogador (Jogador (pJ-1) dJ vJ cJ eJ) mapa) (Jogador (pJ-1) dJ vJ cJ eJ) - alturaDoJogador (posicaoJogador j mapa) j /= 0) = pistasPrioritariasRetas (Jogador 0 dJ vJ cJ eJ) [ (posicaoJogador j mapa), (posicaoJogador (Jogador (pJ+1) dJ vJ cJ eJ) mapa) ]

-- | A função 'arSemIntersecao' roda o 'Jogador' o máximo para os (-90º), pois esta função só é utilizada quando o 'Jogador' não está perto de intersetar o chão 
arSemIntersecao :: Jogador -> Maybe Jogada
arSemIntersecao (Jogador pJ dJ vJ cJ (Ar aJ iJ gJ)) 
    | iJ > (-90) = Just (Movimenta D)  
    | otherwise = Nothing

-- | A função 'arIntersecao' roda o 'Jogador' para igualar a inclinação da 'Peca' por debaixo dele
arIntersecao :: Mapa -> Jogador -> Maybe Jogada
arIntersecao mapa j@(Jogador pJ dJ vJ cJ (Ar aJ iJ gJ))
    | iJ > anguloRampa (posicaoJogador j mapa) = Just (Movimenta D)
    | iJ < anguloRampa (posicaoJogador j mapa) = Just (Movimenta E)
    | otherwise = Nothing 

-- | A função 'disparaCola'  verifica se o 'Jogador' está numa rampa, para poder dispara cola
disparaCola :: Mapa -> Jogador -> Bool
disparaCola mapa j = piso (posicaoJogador j mapa) ==Boost