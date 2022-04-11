
-- | Este módulo define funções genéricas sobre vetores e matrizes, que serão úteis na resolução do trabalho prático.
module Tarefa0_2019li1g042 where

    data Ponto  = Cartesiano Double Double | Polar Double Angulo deriving (Show,Eq)
    type Angulo = Double
    type Vetor  = Ponto
    pToC :: Ponto -> Ponto
    pToC (Polar r a) = Cartesiano (cos ((a*pi)/180)* r) (sin ((a*pi)/180) * r)
    pToC (Cartesiano x y) = Cartesiano x y
    
    
    somaVetores :: Vetor -> Vetor -> Vetor
    somaVetores a b = Cartesiano (x1 + x2) (y1 + y2)
        where 
            Cartesiano x1 y1 = pToC a 
            Cartesiano x2 y2 = pToC b
        
    subtraiVetores :: Vetor -> Vetor -> Vetor
    subtraiVetores a b = Cartesiano (x1 - x2) (y1 - y2)
        where 
            Cartesiano x1 y1 = pToC a 
            Cartesiano x2 y2 = pToC b

    multiplicaVetor :: Double -> Vetor -> Vetor
    multiplicaVetor x a = Cartesiano (x* x1) (x* y1)
        where 
            Cartesiano x1 y1 = pToC a 
    
     
    
    
    type Reta = (Ponto,Ponto)
    
    --Verificar se duas retas se intersetam
    intersetam :: Reta -> Reta -> Bool
    intersetam  (a,b) (c,d) = ta<= 1 && ta>= 0 && tb<= 1 && tb >= 0
        where (ta,tb) = ts (a,b) (c,d)
    
    ts (a,b) (c,d) = (ta,tb)
      where 
        Cartesiano x1 y1 = pToC a 
        Cartesiano x2 y2 = pToC b
        Cartesiano x3 y3 = pToC c 
        Cartesiano x4 y4 = pToC d
        ta = ((y3-y4)*(x1-x3) + (x4-x3)*(y1-y3))/ ((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3))
        tb = ((y1-y2)*(x1-x3) + (x2-x1)*(y1-y3))/ ((x4-x3)*(y1-y2) - (x1-x2)*(y4-y3))
    
    intersecao :: Reta -> Reta -> Ponto
    intersecao (a,b) (c,d) 
        | intersetam (a,b) (c,d) = 
            let (ta,tb)  = ts (a,b) (c,d)
            in (somaVetores a (multiplicaVetor ta (subtraiVetores b a)))
        | otherwise = error "As retas não se intersetam"
    
    eIndiceListaValido :: Int -> [a] -> Bool
    eIndiceListaValido a b = a>=0 && a<length b 
    
    type DimensaoMatriz = (Int, Int)
    type PosicaoMatriz = (Int, Int)
    type Matriz a = [[a]]
    
    
    dimensaoMatriz :: Matriz a -> DimensaoMatriz
    dimensaoMatriz [] = (0,0)
    dimensaoMatriz ([]:xs) = (0,0)
    dimensaoMatriz b = (length  b, length (head b)) 
    
    
    
    ePosicaoMatrizValida :: PosicaoMatriz -> Matriz a -> Bool
    ePosicaoMatrizValida (x1,y1) a = x1 >= 0 && y1 >= 0 && x1 <= (length a) -1 && y1 <= (length (a!! 0)) -1

    
    
    
    normalizaAngulo :: Angulo -> Angulo
    normalizaAngulo x | x > 360 = normalizaAngulo (x-360)
                      | x < 0 = normalizaAngulo (x+360)
                      | otherwise = x
    
    
    encontraIndiceLista ::Int -> [a] -> a
    encontraIndiceLista 0 (x:xs) = x 
    encontraIndiceLista n (x:xs) = encontraIndiceLista (n-1) xs

    atualizaIndiceLista :: Int -> a -> [a] -> [a]
    atualizaIndiceLista 0 c (x:xs) = c:xs
    atualizaIndiceLista n c (x:xs) = x:(atualizaIndiceLista (n-1) c xs)
    
    encontraPosicaoMatriz :: PosicaoMatriz -> Matriz a -> a
    encontraPosicaoMatriz (l, c) m = encontraIndiceLista c (encontraIndiceLista l m)
    
    atualizaPosicaoMatriz :: PosicaoMatriz -> a -> Matriz a -> Matriz a
    atualizaPosicaoMatriz (0,xs) b (c:cs) = (atualizaIndiceLista xs b c) : cs
    atualizaPosicaoMatriz (x,xs) b (c:cs) = c: (atualizaPosicaoMatriz (x-1,xs) b cs)     