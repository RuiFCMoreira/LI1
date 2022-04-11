{- |
= Introdução
Nesta tarefa, o objetivo era comprimir o  máximo possível o mapa do jogo, por outras palavras, escrever o mapa de forma a que ocupasse o minimo espaço possível, não perdendo 
nenhuma da informação.

= Objetivos
O nosso principal objetivo foi transformar um mapa numa lista de 'Instrucao' ('Intrucoes'). No projeto era nos proposto comprimir o mapa segundo os seguintes padrões:
* Padrão Horizontal - se 2 ou mais peças consecutivas da mesma pista fossem iguais, juntavamos as peças numa Instrução só : Repete nvezes [peça]
* Padrão Vertical - se 2 ou mais peças da mesma coluna fossem iguais , juntavamos as peças numa Instrução só , adicionando as pistas onde ocorria essa peça na lista de Inteiros 
onde ocorria a peça
* Padrão Vertical Desfasados - comparar 2 ou mais peças consecutivas ao resto do mapa e se existisse uma sequência igual aplicavamos o Teleporta

= Discusão / Conclusão

Em retrospetiva, achamos os padrões horizontais fáceis. Quando passamos para os verticais sentimos bastante dificuldade do inicio mas acabamos por conseguir. Contundo nao conseguimos 
fazer os Padrões Verticais Desfazados . Apesar de não termos conseguido fazer o Padrão Vertical Desfasado, acreditamos que fizemos um bom trabalho visto que os Padrões Horizontais e 
Verticais otimizam bastante.  
-}
-- | Este módulo define funções comuns da Tarefa 3 do trabalho prático.
module Tarefa3_2019li1g042 where

import LI11920
import System.Random
import Tarefa0_2019li1g042
import Tarefa1_2019li1g042
-- * Testes

-- | Testes unitários da Tarefa 3.
--
-- Cada teste é um 'Mapa'.
testesT3 :: [Mapa]
testesT3 = [(gera 1 2 3),(gera 2 5 1),(gera 5 5 5),(gera 1 4 1),(gera 10 10 10),(gera 5 10 200),t0,t1]
t0 = [[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]]
t1= [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0],[Recta Terra 0,Recta Boost 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0]]
-- * Funções principais da Tarefa 3.

-- | Desconstrói um 'Mapa' numa sequência de 'Instrucoes'.
--
-- __NB:__ Uma solução correcta deve retornar uma sequência de 'Instrucoes' tal que, para qualquer mapa válido 'm', executar as instruções '(desconstroi m)' produza o mesmo mapa 'm'.
--
-- __NB:__ Uma boa solução deve representar o 'Mapa' dado no mínimo número de 'Instrucoes', de acordo com a função 'tamanhoInstrucoes'.
desconstroi :: Mapa -> Instrucoes
desconstroi m  = otimizaVertical m



-- | A Função 'convertePeca' , recebe o nº de pista correspondente de uma Peca e converte uma 'Peca' numa 'Intrucao'
convertePeca :: Int -> Peca ->  Instrucao
convertePeca npista (Recta piso altura) =  Anda [npista] piso
convertePeca npista (Rampa piso ai af) | (af-ai)>0 = Sobe [npista] piso (af-ai)
									   | otherwise = Desce [npista] piso (ai-af)

-- | A Função 'convertePista' transforma uma 'Pista' em 'Intrucoes'
convertePista :: Int -> Pista -> Instrucoes
convertePista npista [] = []
convertePista npista (x:xs) = (convertePeca npista x) : convertePista npista xs

-- | A Função 'convertePista' transforma várias Pistas em 'Intrucoes'
convertePistas :: Int -> Mapa -> Instrucoes 
convertePistas n [] = []
convertePistas n (x:xs) = (convertePista n x) ++ convertePistas (n+1) xs

-- | A Função 'converteMapa' desconstrói um 'Mapa' numa sequência de 'Instrucoes'
converteMapa :: Mapa -> Instrucoes
converteMapa [] = []
converteMapa m@(x:xs) = convertePistas 0 (drop1coluna m)

-- | A Função 'drop1coluna' deita fora a 1 coluna do 'Mapa'
drop1coluna :: Mapa -> Mapa
drop1coluna [] = []
drop1coluna (x:xs) = (drop 1 x) : drop1coluna xs
 


-- * PADRÕES HORIZONTAIS 
-- | A Função 'otimizaHorizontal' desconstrói um 'Mapa' numa sequência de 'Instrucoes' segundo um Padrão Horizontal
otimizaHorizontal :: Mapa -> Instrucoes
otimizaHorizontal [] = []
otimizaHorizontal m  = otimizaHorizontalAux (group' (converteMapa m))

-- | A Função 'group'' agrupa elementos iguais e consecutivos de uma lista de 'Instrucao' ('Instrucoes') 
group' :: Instrucoes -> [Instrucoes]
group' [] = []
group' l = takeWHile' l : group' (dropWhile' l)

-- | A Função 'takeWhile'' é semelhante à função takeWhile pré-definida
takeWHile' :: Instrucoes -> Instrucoes
takeWHile' [] = []
takeWHile' [x] = [x]
takeWHile' (x:y:xs) | x==y = x : takeWHile' (y:xs)
				    | otherwise = [x]

-- | A Função 'dropWhile'' é semelhante à função dropWhile pré-definida
dropWhile' ::  Instrucoes -> Instrucoes
dropWhile' [] = []
dropWhile' [x] = []
dropWhile' (x:y:xs) | x==y = dropWhile' (y:xs)
				    | otherwise = (y:xs)

-- | A Função 'length'' é semelhante à função length pré-definida e calcula o comprimento de uma lista de 'Instrucao' ('Instrucoes')
length' :: Instrucoes -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- | A Função 'nrepete' transforma uma lista de 'Instrucao' com os elementos todos iguais numa só 'Instrucao'
nrepete :: Instrucoes -> Instrucao
nrepete l@(x:xs) = Repete (length' l) [x]

-- | A Função 'otimizaHorizontalAux' tranforma uma lista de 'Intrucoes' com elementos iguais (lista essa calculada pela função 'group') em 'Instrucoes'
otimizaHorizontalAux :: [Instrucoes] -> Instrucoes
otimizaHorizontalAux [] = []
otimizaHorizontalAux (x:xs) | length' x > 1 =  nrepete x : otimizaHorizontalAux xs
							| otherwise = aux x : otimizaHorizontalAux xs

-- | A Função 'aux' transforma 'Instrucoes' com 1 só elemento numa 'Instrucao'
aux:: Instrucoes -> Instrucao
aux [x] = x



-- * PADRÕES VERTICAIS
-- | A Função 'otimizaVertical' desconstrói um 'Mapa' numa sequência de 'Instrucoes' segundo um Padrão Horizontal e um Padrão Vertical
otimizaVertical :: Mapa -> Instrucoes
otimizaVertical m | length m <=1 = otimizaHorizontal m
				  | otherwise =otimizaHorizontalAux (group' (converteLista (otimizaMapa (converteMapa' 0 (drop1coluna m)))))--

-- | A Função 'converteMapa'' converte um 'Mapa' de 'Pecas' para uma Matriz de 'Instrucao'
converteMapa' :: Int -> Mapa -> [Instrucoes] 
converteMapa' n [] = []
converteMapa' n (x:xs) = (convertePista n x) : converteMapa' (n+1) xs

-- | A Função 'coluna' devolve a coluna respestiva ao número que introduzimos na função
coluna :: Int -> [Instrucoes] -> Instrucoes
coluna _ [] = []
coluna 0 (x:xs) = (head x) : coluna 0 xs
coluna n (x:xs) = coluna (n-1) (drop1coluna' (x:xs))

-- | A Função 'drop1coluna' deita fora a 1 coluna da '[Instrucoes]'
drop1coluna' :: [Instrucoes] -> [Instrucoes]
drop1coluna' [] = []
drop1coluna' (x:xs) | length x == 1 = []
					| otherwise = (drop 1 x) : drop1coluna' xs

-- | A Função 'instrucaoIgualInstrucao' verifica se uma 'Instrucao' é igual a outra 'Instrucao'
instrucaoIgualInstrucao :: Instrucao -> Instrucao -> Bool
instrucaoIgualInstrucao a@(Anda (x:xs) pisox) b@(Anda (y:ys) pisoy) = pisox==pisoy
instrucaoIgualInstrucao (Sobe (x:xs) pisox alturax) (Sobe (y:ys) pisoy alturay) = pisox==pisoy && alturax==alturay 
instrucaoIgualInstrucao (Desce (x:xs) pisox alturax) (Desce (y:ys) pisoy alturay) = pisox==pisoy && alturax==alturay
instrucaoIgualInstrucao _ _ = False  

-- | A Função 'instrucaoIgualInstrucao' verifica se uma 'Instrucao' pertence a 'Instrucoes'
instrucaoIgualInstrucoes :: Instrucao -> Instrucoes -> Bool
instrucaoIgualInstrucoes y [] = False
instrucaoIgualInstrucoes y (x:xs) = (instrucaoIgualInstrucao y x) || (instrucaoIgualInstrucoes  y xs)

-- | A Função 'instrucoesIgual' recebe uma 'Instrucao' e 'Instrucoes' e junta a 'Instrucao' a todos os elementos de 'Instrucoes' que sejam iguais 
instrucoesIgual :: Instrucao -> Instrucoes -> Instrucoes
instrucoesIgual y [] = [y]
instrucoesIgual y (x:xs) | instrucaoIgualInstrucao y x = instrucoesIgual (juntaInstrucao y x) xs
						 | otherwise = (x :instrucoesIgual y xs )

-- | A Função 'juntaInstrucao' recebe 2 'Instrucoes' iguais e junta as 2 'Instrucoes' numa só
juntaInstrucao :: Instrucao -> Instrucao -> Instrucao
juntaInstrucao (Anda (x:xs) pisox) (Anda (y:ys) pisoy) = (Anda ((x:xs)++(y:ys)) pisox)
juntaInstrucao (Sobe (x:xs) pisox alturax) (Sobe (y:ys) pisoy alturay) = (Sobe ((x:xs)++(y:ys)) pisox alturax)
juntaInstrucao (Desce (x:xs) pisox alturax) (Desce (y:ys) pisoy alturay) = (Desce ((x:xs)++(y:ys)) pisox alturax)

-- | A Função 'otimizaColuna' otimiza uma coluna
otimizaColuna :: Instrucoes -> Instrucoes
otimizaColuna [] = []
otimizaColuna (x:xs) | instrucaoIgualInstrucoes x xs = (otimizaColuna (instrucoesIgual x xs))
					 | otherwise = x : otimizaColuna xs


-- | A Função 'otimizaMapa' otimiza Mapa , coluna a coluna
otimizaMapa :: [Instrucoes] -> [Instrucoes]
otimizaMapa [] = []
otimizaMapa m  = (otimizaColuna (coluna 0 m)) : (otimizaMapa (drop1coluna' m))

-- | A Função 'converteLista' concatena as listas de uma lista de 'Instrucoes'
converteLista :: [Instrucoes] -> Instrucoes
converteLista [] = []
converteLista (x:xs) = x ++ converteLista xs
