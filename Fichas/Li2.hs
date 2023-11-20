data Movimento = Norte | Sul | Este | Oeste deriving (Show, Eq) 
type Ponto = (Double,Double)

move :: Ponto -> Movimento -> Ponto
move (x,y) m
        | m == Norte = (x, y)
        | m == Sul = (x, y-1)
        | m == Este = (x+1, y)
        | m == Oeste = (x-1, y)

dist :: Ponto -> Ponto -> Double
dist (m,s) (x,y) = sqrt( (m-x)^2+(s-y)^2)

janela :: Ponto -> Double -> Ponto
janela (x,y) l = (x, y-l)

type Velocidade = Double
type Tempo = Double
movev :: Ponto -> Velocidade -> Tempo -> Ponto
movev (x,y) v t = let d = v*t 
                 in (x+d, y)

controi :: Ponto -> Double -> Figura
constroi p r = Circulo p r 

data Figura = Circulo Ponto Double | Rectangulo Ponto Ponto deriving (Show,Eq)

dentro :: Ponto -> Figura -> Bool
dentro (x,y) ( Circulo(c1,c2) r ) = dist(x,y) (c1,c2) <= r
dentro (x,y) ( Rectangulo(x1,y1)(x2,y2) ) = 
       (x<=x1) && (x2<= x) && (y1<=y) && (y2<=y)
