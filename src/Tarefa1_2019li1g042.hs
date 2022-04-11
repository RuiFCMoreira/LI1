{- |
	Esta Tarefa tem como objetivo gerar um mapa semi-aleatório utilizando uma seed. Esta foi uma das Tarefas mais simples, pelo que não houve muito problema a fazê -la
-}


-- | Este módulo define funções comuns da Tarefa 2 do trabalho prático.
module Tarefa1_2019li1g042 where

import LI11920
import System.Random

-- * Testes

-- | Testes unitários da Tarefa 1.
--
-- Cada teste é um triplo (/número de 'Pista's/,/comprimento de cada 'Pista' do 'Mapa'/,/semente de aleatoriedades/).

-- * Funções pré-definidas da Tarefa 1.
testesT1 :: [(Int,Int,Int)]
testesT1 = [(1,1,1),(1,6,2),(6,1,5),(9,10,7),(4,2,2), (10,12,27), (50,50,50), (1,2,67), (2,1,1089),(4,5,6), (0,1,2),(3,1,0)]


-- | A função 'geraAleatorios' cria uma lista com n números inteiros segundo uma semente
geraAleatorios :: Int -> Int -> [Int]
geraAleatorios z seed = take z (randomRs (0,9) (mkStdGen seed))



-- * Funções principais da Tarefa 1.
-- | A função 'gera' gera um mapa
gera :: Int -> Int -> Int -> Mapa
gera npistas comprimento semente = geraPistas npistas comprimento (transformaPar (geraAleatorios (2*((npistas*comprimento)-npistas)) semente)) 



--  | A função 'geraPistas' gera várias Pistas
geraPistas :: Int -> Int -> [(Int,Int)] -> Mapa
geraPistas 0 comprimento l = []
geraPistas npistas comprimento l = (geraPista comprimento l):(geraPistas (npistas-1) comprimento (drop (comprimento-1) l))

-- | A função 'geraPista' gera 1 Pista
geraPista :: Int -> [(Int,Int)] -> Pista 
geraPista 1 l = (Recta Terra 0) : []
geraPista comprimento l = (Recta Terra 0) : (criaLinhaPecas (Recta Terra 0) (comprimento-1) l)

-- | A função 'transformaPar' converte uma Lista para uma Lista de pares
transformaPar :: [Int] ->  [(Int,Int)]
transformaPar []  = []
transformaPar (x:y:t) = (x,y) : transformaPar t

-- | A função 'criaLinhaPecas' cria uma Linha de Peças (Sem a primeira da pista)
criaLinhaPecas :: Peca -> Int -> [(Int,Int)] -> [Peca]
criaLinhaPecas p comprimento []  = []
criaLinhaPecas p 0 l = []
criaLinhaPecas p comprimento (x:xs) = (peca p x)  : criaLinhaPecas (peca p x) (comprimento-1) xs

-- | A função 'peca' recebe a Peca anterior e o par correspondente a uma peca e da essa peca 
peca :: Peca ->  (Int,Int) -> Peca
peca (Recta p a) (x,y) = gamaAltura y a (gamaPiso p x)
peca (Rampa p ai af) (x,y) = gamaAltura y af (gamaPiso p x)

-- | A função 'gamaPiso' atribui Piso à Peça
gamaPiso :: Piso -> Int -> Piso
gamaPiso p x | x==0 || x==1 = Terra 
             | x==2 || x==3 = Relva
             | x==4         = Lama 
             | x==5         = Boost 
			 | x>=6 && x<=9 = p  
	 
-- | A função 'gamaAltura' cria uma Peça com altura e Piso
gamaAltura :: Int ->  Int ->  Piso -> Peca
gamaAltura gama alturaAnterior pisoAtual | gama == 0 || gama == 1 = Rampa pisoAtual alturaAnterior (alturaAnterior + (gama+1))
										 | gama >= 2 && gama <= 5 = calculaDescida gama alturaAnterior pisoAtual
										 | gama >= 6 && gama <=9 = Recta pisoAtual alturaAnterior


-- | A função 'calculaDescida'  gcalcula uma peca que seja uma rampa descendente 								 
calculaDescida :: Int ->  Int ->  Piso -> Peca
calculaDescida gama alturaAnterior pisoAtual | alturaAnterior == (subtraiAlturas alturaAnterior (gama-1)) =  Recta pisoAtual alturaAnterior
										 	 | otherwise = Rampa pisoAtual alturaAnterior (subtraiAlturas alturaAnterior (gama-1))

-- | o 'subtraiAlturas' subtrai duas alturas
subtraiAlturas :: Int -> Int -> Int
subtraiAlturas  x y |x <= y = 0
					|x > y = (x-y)