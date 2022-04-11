{- |

= Introdução
Esta tarefa foi a tarefa na qual tivemos que usar o gloss para criar a parte visual do jogo e implementar os mecanismos deste mesmo jogo, utilizando as tarefas 2 e 4 para
aplicar a fisica nas motas.

= Objetivos
O nosso principal objetivo nesta tarefa era criar um jogo minimamente apelativo e funcional. Tentamos colocar imagens o mais realistas possiveis para quem nunca jogou o jogo,
perceber minimamente o que cada coisa é e como funciona o jogo. Por exemplo, as motas que implementamos no nosso jogo são idênticas às do jogo original. Para que o jogo ficasse 
minimamente funcional e fosse percetível ao jogador que está a acontecer, tentamos desenhar o mapa com uma perspetiva diferente, para que o jogador pudesse visualizar as rampas,
aproximando assim de uma perspetiva 3D.
Nesta Tarefa, tinhamos também a intenção de criar um menu. 

= Discussão e Conclusão
Esta Tarefa, para nós foi das mais dificeis, visto que tivemos de aprendrer a usar o gloss e tivemos bastantes dificuldades, pois é um pouco diferente do que tinhamos feito.
Nesta tarefa, tentamos criar um mapa com uma perspetiva diferente, mais difícil do que um mapa normal em 2D, tendo conseguido este objetivo. 
Contudo nesta Tarefa, não conseguimos implementar os mecanismos do jogo. Não conseguimos perceber como o fazer. Como não conseguimos implementar os mecanismos do jogo acabamos 
por desenhar apenas o mapa e os jogadores na primeira peça. Durante o ficheiro deixamos em comentário algumas tentativas nossas de implementar minimamente os mecanismos do 
jogo.
-}
module Main where

    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import LI11920
    import Tarefa2_2019li1g042
    import Tarefa0_2019li1g042
    import Tarefa4_2019li1g042

-- * CORES
-- | As seguintes funções são definições de cores que iremos usar mais à frente nesta tarefa.    

    castanhoC :: Color
    castanhoC = makeColorI 194 166 120 1000

    castanhoE :: Color
    castanhoE = makeColorI 101 63 18 1000

    castanhoCT :: Color
    castanhoCT = makeColorI 149 127 92 1000

    castanhoET :: Color
    castanhoET = makeColorI 82 51 13 1000

    cinzentoT :: Color
    cinzentoT = makeColorI 63 63 67 1000

    verdeT :: Color
    verdeT = makeColorI 57 99 30 1000

-- * DEFINIÇÃO DAS RAMPAS
-- | triângulo da rampa ,que serve para dar noção de prespetiva
    trianguloRampa :: Float -> Float -> Picture
    trianguloRampa ai af= Polygon [(0,0),(ladoPeca,0),(ladoPeca,af*ladoPeca),(0,ai*ladoPeca),(0,0)]

-- | Poligono da Rampa 
    rampaaux :: Float -> Float -> Picture
    rampaaux ai af = Polygon [(0,0),(ladoPeca,0),(ladoPeca,ladoPeca+(af*ladoPeca)),(0,ladoPeca+(ai*ladoPeca)),(0,0)]

{- | As seguintes funções chamam as 2 funções anteriores e junta as duas 'Picture' e pinta a figura dada pela 'trianguloRampa' com uma cor mais escura e a figura dada pela 
'rampaaux' com uma cor mais clara de modo a ficar melhor esteticamente e de maneira a dar uma sensação de prespetiva e posiocinamento  -}
    rampa_terra :: Float -> Float ->Picture
    rampa_terra ai af = Pictures [(Color castanhoC (rampaaux ai af)),(Color castanhoCT (trianguloRampa ai af))]

    rampa_boost :: Float -> Float ->Picture
    rampa_boost ai af = Pictures [(Color white (rampaaux ai af)),(Color cinzentoT (trianguloRampa ai af))]

    rampa_relva :: Float -> Float ->Picture
    rampa_relva ai af = Pictures [(Color green (rampaaux ai af)),(Color verdeT (trianguloRampa ai af))]

    rampa_lama :: Float -> Float -> Picture
    rampa_lama ai af = Pictures [(Color castanhoE (rampaaux ai af)),(Color castanhoET (trianguloRampa ai af))]


-- * DEFINIÇÃO DAS RETAS
-- | As seguintes funções definem as retas quando a altura não é 0, de modo a dar uma sensação de prespetiva e posiocinamento 
    reta_terra :: Float -> Picture
    reta_terra a = Pictures [(Color castanhoC (Polygon [(0,0),(ladoPeca,0),(ladoPeca,(a*ladoPeca)+ladoPeca),(0,(a*ladoPeca)+ladoPeca),(0,0)])),(Color castanhoCT (Polygon [(0,0),(ladoPeca,0),(ladoPeca,(ladoPeca*a)),(0,(ladoPeca*a)),(0,0)]))]

    reta_boost :: Float -> Picture
    reta_boost a = Pictures [(Color white (Polygon [(0,0),(ladoPeca,0),(ladoPeca,(a*ladoPeca)+ladoPeca),(0,(a*ladoPeca)+ladoPeca),(0,0)])),(Color cinzentoT (Polygon [(0,0),(ladoPeca,0),(ladoPeca,(ladoPeca*a)),(0,(ladoPeca*a)),(0,0)]))]

    reta_relva :: Float -> Picture
    reta_relva a = Pictures [(Color green (Polygon [(0,0),(ladoPeca,0),(ladoPeca,(a*ladoPeca)+ladoPeca),(0,(a*ladoPeca)+ladoPeca),(0,0)])),(Color verdeT (Polygon [(0,0),(ladoPeca,0),(ladoPeca,(ladoPeca*a)),(0,(ladoPeca*a)),(0,0)]))]

    reta_lama :: Float -> Picture
    reta_lama a = Pictures [(Color castanhoE (Polygon [(0,0),(ladoPeca,0),(ladoPeca,(a*ladoPeca)+ladoPeca),(0,(a*ladoPeca)+ladoPeca),(0,0)])),(Color castanhoET (Polygon [(0,0),(ladoPeca,0),(ladoPeca,(ladoPeca*a)),(0,(ladoPeca*a)),(0,0)]))]




   -- | ESTADO JOGO
   -- | alguns exemplos de mapas e jogadores para testar 
    mapa103 = [[Recta Terra 0,Recta Boost 0,Recta Boost 0,Recta Boost 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Rampa Lama 0 2,Rampa Lama 2 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Lama 0,Recta Terra 0,Rampa Terra 0 2,Rampa Boost 2 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0],[Recta Terra 0,Rampa Terra 0 2,Rampa Relva 2 0,Recta Boost 0,Recta Boost 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Recta Boost 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 1,Rampa Relva 1 0,Recta Relva 0,Rampa Relva 0 1],[Recta Terra 0,Recta Lama 0,Rampa Lama 0 2,Rampa Lama 2 0,Rampa Relva 0 1,Rampa Relva 1 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Relva 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Rampa Boost 0 2,Rampa Terra 2 4,Recta Relva 4,Rampa Terra 4 6,Recta Terra 6,Recta Relva 6,Recta Lama 6,Recta Boost 6,Rampa Terra 6 2,Rampa Terra 2 4],[Recta Terra 0,Rampa Relva 0 1,Recta Terra 1,Rampa Lama 1 0,Recta Lama 0,Recta Lama 0,Rampa Terra 0 1,Rampa Lama 1 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Lama 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Rampa Terra 0 2,Recta Relva 2,Rampa Relva 2 0,Recta Relva 0,Recta Terra 0,Recta Terra 0,Recta Terra 0,Recta Relva 0,Recta Boost 0]]
    mapa102 = [[Recta Terra 0,Recta Terra 0,Recta Boost 0,Recta Terra 0,Recta Relva 0,Recta Relva 0,Rampa Relva 0 1],[Recta Terra 0,Rampa Boost 0 1,Rampa Boost 1 0,Recta Lama 0,Rampa Lama 0 1,Rampa Lama 1 3,Rampa Terra 3 1],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Recta Terra 2,Rampa Terra 2 1,Rampa Terra 1 0,Rampa Boost 0 1],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Rampa Relva 0 1,Recta Boost 1,Recta Boost 1,Rampa Lama 1 0],[Recta Terra 0,Recta Terra 0,Recta Lama 0,Recta Lama 0,Rampa Boost 0 2,Rampa Terra 2 0,Recta Terra 0],[Recta Terra 0,Recta Terra 0,Rampa Relva 0 1,Recta Terra 1,Recta Boost 1,Recta Terra 1,Rampa Lama 1 0],[Recta Terra 0,Recta Terra 0,Rampa Terra 0 2,Recta Relva 2,Recta Relva 2,Rampa Relva 2 3,Rampa Terra 3 4]]
    mapa101 = [[Recta Terra 0, Rampa Relva 0 1, Recta Terra 1], [Recta Terra 0, Recta Relva 0, Rampa Terra 0 1], [Recta Terra 0, Rampa Relva 0 1, Recta Lama 1]]
    mapa100 = [[Recta Terra 0, Recta Relva 0, Recta Terra 0,Recta Terra 0], [Recta Terra 0, Recta Relva 0,Recta Terra 0, Rampa Terra 0 1], [Recta Terra 0, Recta Relva 0,Recta Terra 0, Recta Lama 2]]
    ljogadores = [(Jogador 0 0 0 5 (Chao True )),(Jogador 1 0 0 5 (Chao True )),(Jogador 2 0 0 5 (Chao True )),(Jogador 3 0 0 5 (Chao True ))] 
  -- | Estado incial do jogo  
    estadoInicial :: Estado
    estadoInicial = (Estado mapa103 ljogadores) 

-- | A 'desenhaEstado' desenha o jogo na janela do gloss
    desenhaEstado :: Estado -> [Picture] -> [Picture]
    desenhaEstado (Estado m lj@((Jogador _ d _ _ _):_)) li = (desenhaMapa m xi yi li) ++ (desenhaJogadores m jogadores (listaEntre2Indices 4 7 li)) 
--  desenhaEstado (Estado m lj@((Jogador _ d _ _ _):_)) li = (desenhaMapa m (xi-d1) yi li) ++ (desenhaJogadores m jogadores (listaEntre2Indices 4 7 li)) 
                            where 
                                -- d1 = realToFrac d     -- esta alteração fazia o mapa movimentar-se em relação à posição do primeiro jogador
                                jogadores = take 4 lj

-- | A 'listaEntre2Indices' recebe dois Inteiros e uma lista de Picture e da uma parte dessa lista 
    listaEntre2Indices :: Int -> Int -> [Picture] -> [Picture]
    listaEntre2Indices x y li | x == y = [encontraIndiceLista x li]
                              | otherwise = [encontraIndiceLista x li] ++ listaEntre2Indices (x+1) y li  
                        

    -- | Esta função executa algo no jogo com base na tecla premida pelo utilizador.    
    reageEvento :: Event -> Estado -> Estado
    reageEvento (EventKey (Char 'w') Down _ _) e = jogada 0 (Movimenta C) e         -- tentamos implementar alguns mecanismos de movimentar os jogadores mas nao surtiu resultado
    reageEvento (EventKey (Char 'd') Down _ _) e = jogada 0 (Movimenta D) e
    reageEvento (EventKey (Char 'a') Down _ _) e = jogada 0 (Movimenta E) e
    reageEvento (EventKey (Char 's') Down _ _) e = jogada 0 (Movimenta B) e
    reageEvento _ e = e -- ignora qualquer outro evento

-- | Esta funçao define a passagem do tempo no jogo  
    reageTempo :: Float -> Estado -> Estado 
    reageTempo n (Estado m (j:t)) = (Estado m (passoLista (realToFrac n) m (j:t)))


    passoLista :: Double -> Mapa -> [Jogador] -> [Jogador]
    passoLista t m [] = []
    passoLista t m (j:js) = passo t m j : passoLista t m js 

   -- | DESENHA MAPA
-- | (xi,yi) é o ponto onde vai começar a ser desenhado o jogo
    xi :: Float
    xi = -600

    yi :: Float 
    yi = -100

-- | Tamanho do lado de  uma Peça
    ladoPeca :: Float 
    ladoPeca = 100

-- | Esta função desenha um Mapa
    desenhaMapa :: Mapa -> Float -> Float -> [Picture] -> [Picture]
    desenhaMapa [] _ _ _ = []
    desenhaMapa (h:t) xi yi li = desenhaPista h xi yi li ++ desenhaMapa t xi (yi-ladoPeca) li

-- | Esta funçao desenha uma Pista
    desenhaPista :: Pista -> Float -> Float -> [Picture] -> [Picture]
    desenhaPista [] _ _ _ = []
    desenhaPista (h:t) x y li = desenhaPeca h x y li ++ desenhaPista t (x+ladoPeca) y li

-- | Esta função desenha uma Peça
    desenhaPeca :: Peca -> Float -> Float -> [Picture] -> [Picture]
    desenhaPeca (Recta Terra a) x y (reta_terra0:reta_boost0:reta_relva0:reta_lama0:mota1:mota2:mota3:mota4:[]) | a==0 = [Translate (x+(0.5*ladoPeca)) (y+(0.5*ladoPeca)) reta_terra0]
                                                                                                                | otherwise = [Translate x y (reta_terra (realToFrac a)),Translate (x+(0.5*ladoPeca)) (y+(ladoPeca*(realToFrac a))+(0.5*ladoPeca)) reta_terra0]
    desenhaPeca (Recta Relva a) x y (reta_terra0:reta_boost0:reta_relva0:reta_lama0:mota1:mota2:mota3:mota4:[]) | a==0 = [Translate (x+(0.5*ladoPeca)) (y+(0.5*ladoPeca)) reta_relva0]
                                                                                                                | otherwise = [Translate x y (reta_relva (realToFrac a)),Translate (x+(0.5*ladoPeca)) (y+(ladoPeca*(realToFrac a))+(0.5*ladoPeca)) reta_relva0]
    desenhaPeca (Recta Lama a) x y (reta_terra0:reta_boost0:reta_relva0:reta_lama0:mota1:mota2:mota3:mota4:[])  | a==0 = [Translate (x+(0.5*ladoPeca)) (y+(0.5*ladoPeca)) reta_lama0]
                                                                                                                | otherwise = [Translate x y (reta_lama (realToFrac a)),Translate (x+(0.5*ladoPeca)) (y+(ladoPeca*(realToFrac a))+(0.5*ladoPeca)) reta_lama0]
    desenhaPeca (Recta Boost a) x y (reta_terra0:reta_boost0:reta_relva0:reta_lama0:mota1:mota2:mota3:mota4:[]) | a==0 = [Translate (x+(0.5*ladoPeca)) (y+(0.5*ladoPeca)) reta_boost0]
                                                                                                                | otherwise = [Translate x y (reta_boost (realToFrac a)),Translate (x+(0.5*ladoPeca)) (y+(ladoPeca*(realToFrac a))+(0.5*ladoPeca)) reta_boost0]
    desenhaPeca (Rampa Terra ai af) x y (reta_terra0:reta_boost0:reta_relva0:reta_lama0:mota1:mota2:mota3:mota4:[]) = [Translate x y (rampa_terra (realToFrac ai) (realToFrac af))]
    desenhaPeca (Rampa Relva ai af) x y (reta_terra0:reta_boost0:reta_relva0:reta_lama0:mota1:mota2:mota3:mota4:[]) = [Translate x y (rampa_relva (realToFrac ai) (realToFrac af))]
    desenhaPeca (Rampa Lama ai af) x y (reta_terra0:reta_boost0:reta_relva0:reta_lama0:mota1:mota2:mota3:mota4:[]) = [Translate x y (rampa_lama (realToFrac ai) (realToFrac af))]
    desenhaPeca (Rampa Boost ai af) x y (reta_terra0:reta_boost0:reta_relva0:reta_lama0:mota1:mota2:mota3:mota4:[]) = [Translate x y (rampa_boost (realToFrac ai) (realToFrac af))]

    -- * DESENHA JOGADORES
-- | Esta função desenha uma lista de jogadores
    desenhaJogadores :: Mapa -> [Jogador] -> [Picture] -> [Picture]
    desenhaJogadores _ [] _ = []
    desenhaJogadores _ _ [] = []
    desenhaJogadores m (h:t) iJ@(i1:is) = (desenhaJogador (posicaoJogador h m) h i1) : (desenhaJogadores m t is)
    

-- | Esta funçao desenha  um jogador
    
  --  desenhaJogador p (Jogador pJ dJ _ _ _) i1 = Translate (xi+(realToFrac dJ)*ladoPeca) (yi-(realToFrac pJ)*ladoPeca+(0.5*ladoPeca)) i1 
  
-- | Esta tentativa de função , desenhava o jogador tendo. em conta a peça em que se encontrava para saber a inclinação e tendo em conta a sua pista e distância. Contundo ao aplicar
-- esta função, o primeiro jogador desaparecia e dava um erro de "Non-exhaustive patterns in function encontraIndiceLista".
    desenhaJogador :: Peca -> Jogador -> Picture -> Picture
    desenhaJogador p@(Recta _ _) j@(Jogador pJ dJ vJ cJ (Chao _)) i1 = Translate (xi+(realToFrac dJ)*ladoPeca) (yi-(realToFrac pJ)*ladoPeca+(0.5*ladoPeca)) (rotacao p j i1)           
    desenhaJogador p@(Rampa _ _ _) j@(Jogador pJ dJ vJ cJ (Chao _)) i1 = Translate (xi+(realToFrac dJ)*ladoPeca) (yi-(realToFrac pJ)*ladoPeca+(ladoPeca)) (rotacao p j i1)
    desenhaJogador p j@(Jogador pJ dJ vJ cJ (Ar a i g)) i1= Translate (xi+(realToFrac dJ)*ladoPeca) (yi-(realToFrac pJ)*ladoPeca+(0.5*ladoPeca)+(realToFrac a)*ladoPeca) (imagem)
                            where 
                                imagem = Rotate (-(realToFrac a)) i1

 -- | Esta função roda a mota conforme a inclinaçao da peca recebida
    rotacao :: Peca -> Jogador -> Picture -> Picture
    rotacao p@(Rampa _ ai af) (Jogador pJ dJ vJ cJ (Chao _)) i = Rotate ( -(anguloRampaGraus2 p)) i
    rotacao _ _ i = Rotate 0 i 

   -- | Esta função (idêntica à da tarefa 4 mas devolve Float) dá o ângulo duma rampa em graus
    anguloRampaGraus2 :: Peca -> Float
    anguloRampaGraus2 x = realToFrac (((anguloRampa x) * 180)/pi)


-- * ESTADO GLOSS
-- | o Estado Gloss permite andarmos com uma lista de imagens que carregamos no inicio
    type EstadoGloss = (Estado,[Picture])

-- | As seguintes funçoes são identicas as funcões definidas a cima ('estadoInicial','desenhaEstado','reageEvento','reageTempo') mas esta  permite "andar"  com as imagens que carregamos no inicio para sempre que  precisarmos   
    estadoGlossInicial :: [Picture] -> EstadoGloss
    estadoGlossInicial li = (estadoInicial,li)

    desenhaEstadoGloss :: EstadoGloss -> Picture
    desenhaEstadoGloss (e,li) = Pictures(desenhaEstado e li)

    reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
    reageEventoGloss ev (e,li) = (reageEvento ev e , li)

    reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
    reageTempoGloss t (e,li) = ((reageTempo t e), li)

-- | O framerate do jogo, isto é, quantas vezes a função 'reageTempo' é executada num segundo.
    fr :: Int
    fr = 15

-- | A dimensão da janela do jogo    
    dm :: Display
    dm = FullScreen
    -- InWindow "Novo Jogo" (800, 800) (0, 0)     

-- | Função 'main', permite que o jogo corra, e permite importar as imagens para o jogo.
    main :: IO ()
    main = do 
        reta_boost0 <- loadBMP "Boost.bmp" 
        reta_terra0 <- loadBMP "terra.bmp"
        reta_relva0 <- loadBMP "relva.bmp" 
        reta_lama0 <- loadBMP "lama.bmp" 
        mota1 <- loadBMP "motaVermelha.bmp"
        mota2 <- loadBMP "motaAmarela.bmp"
        mota3 <- loadBMP "motaVerde.bmp"
        mota4 <- loadBMP "motaAzul.bmp"
        play dm              -- janela onde irá correr o jogo
            black            -- côr do fundo da janela
            fr              -- frame rate
            (estadoGlossInicial [reta_terra0,reta_boost0,reta_relva0,reta_lama0,mota1,mota2,mota3,mota4]) -- estado inicial
            desenhaEstadoGloss   -- desenha o estado do jogo
            reageEventoGloss     -- reage a um evento
            reageTempoGloss      -- reage ao passar do tempo   






