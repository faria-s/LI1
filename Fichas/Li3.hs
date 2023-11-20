
import Data.Char

addList :: [Int] -> Int -> [Int]
addList [] _ = []
addList (x:xs) t = (x+t) : (addList xs t)

removeString ::  [String] -> Char -> [String]
removeString [] _ = []
removeString (h:t) c 
    |h == "" = removeString t c
    |c == head h = removeString t c
    | c /= head h = h : removeString t c 

paresInt :: [(Int,Int)] -> Int -> [(Int,Int)]
paresInt [] _ = []
paresInt ((x,t):xs) y = (x+y,t) : (paresInt xs y)

calcmaior :: [(Int,Int)] -> Int
calcmaior [(x,y)] = y
calcmaior ((x,y):t) 
  | y > (calcmaior t) = y
  | otherwise = calcmaior t

deslocaDireitaN :: [Int] -> Int -> [Int] 
deslocaDireitaN [] _ = []
deslocaDireitaN l 0 = l 
deslocaDireitaN l n = deslocaDireitaN ((last l): init l) (n-1)

-- | recebe caracteres digitos
nextDigit :: Char -> Char 
nextDigit '9' = '0'
nextDigit c = if (isDigit c) then chr (ord c +1) else c 

{-nextvog :: [Char] -> Char
nextvog [] = []
nextvog  (x:y) = -}

type Nome = String
type Coordenada = (Int, Int)
data Movimento= N | S | E | W  deriving (Show,Eq) -- norte, sul, este, oeste
type Movimentos = [Movimento]
data PosicaoPessoa = Pos Nome Coordenada deriving (Show,Eq)

posicao:: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao p [] = p
posicao (Pos t (x,y)) ((u):m)
  | u == N = posicao (Pos t (x, y+1)) m
  | u == S = posicao (Pos t (x, y-1)) m
  | u == W = posicao (Pos t (x-1, y)) m 
  | u == E = posicao (Pos t (x+1, y)) m 

posicaoM:: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicaoM [] _ = []
posicaoM ((Pos t (x,y)):i) (u:m)
  | u == N = (Pos t (x, y+1)) : posicaoM i m
  | u == S = (Pos t (x, y-1)) : posicaoM i m 
  | u == W = (Pos t (x-1, y)) : posicaoM i m 
  | u == E = (Pos t (x+1, y)) : posicaoM i m 


posicaoMs:: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa]
posicaoMs [] _ = []
posicaoMs (p:t) lm = (posicao p lm) : posicaoMs t lm

{-|posicaoMs' :: [PosicaoPessoa] -> Movimentos -> [PosicaoPessoa] 
posicaoMs' lp [] = lp
posicaoMs' lp (m:lm) = posicaoMs' (posicaoM lp m) lm -}

maisNorte :: [PosicaoPessoa] -> (Int,Int)
maisNorte [Pos _ c] = c
maisNorte ((Pos nome (x,y)):t) = 
  let (xn,yn) = maisNorte t
  in if y > yn then (x,y) else (xn,yn)

maisNorte' :: [PosicaoPessoa] -> (Int,Int)
maisNorte' [Pos _ c] = c
maisNorte' ((Pos n (x,y)):(Pos n2 (x1,y1)):lp)
 | y < y1 = maisNorte' ((Pos n2 (x1,y1)):lp)
 |otherwise = maisNorte' ((Pos n (x,y)):lp)

pessoaNorte :: [PosicaoPessoa] -> [Nome]
pessoaNorte [] = []
pessoaNorte ((Pos nome (x,y)):lp) = pessoaNorteAux lp y [nome]
 where pessoaNorteAux [] y listanomes = listanomes
       pessoaNorteAux ((Pos nome1 (x1,y1):lp)) y listanomes 
        | y > y1 = pessoaNorteAux lp y listanomes
        | y == y1 = pessoaNorteAux lp y (nome1:listanomes)
        | y < y1 =  pessoaNorteAux lp y1 [nome1]

pessoaNorte' :: [PosicaoPessoa] -> [Nome]
pessoaNorte' [] = []
pessoaNorte' lp = let (xn,yn) = maisNorte' lp
                  in filtraPessoas yn

filtraPessoas :: [PosicaoPessoa] -> (Int,Int) -> [Nome]
filtraPessoas ((Pos nome (x,y)):t) yn 
  | y == yn = nome : filtraPessoas lp yn
  | otherwise = filtraPessoas lp yn 



{- Função para calcular a posição de uma pessoa após uma sequência de movimentos
posicao :: PosicaoPessoa -> Movimentos -> PosicaoPessoa
posicao (Pos nome (x, y)) [] = Pos nome (x, y)  -- Caso base: sem movimentos, a posição não muda
posicao (Pos nome (x, y)) (movimento:movimentosRestantes) =
    case movimento of
        N -> posicao (Pos nome (x, y + 1)) movimentosRestantes
        S -> posicao (Pos nome (x, y - 1)) movimentosRestantes
        E -> posicao (Pos nome (x + 1, y)) movimentosRestantes
        W -> posicao (Pos nome (x - 1, y)) movimentosRestantes -}






