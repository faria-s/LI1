{- mat :: [[Int]]
mat [[1,2,3],[4,5,6],[7,8,9]] -}

trocarPrimUlt :: [a] -> [a]
trocarPrimUlt [] = []
trocarPrimUlt (x:xs) = (last xs) : ((init xs)++[x])

trocarLinhas :: [[Int]] -> [[Int]]
trocarLinhas m = trocarPrimUlt m

trocarColuna :: [[Int]] -> [[Int]]
trocarColuna [] = []
trocarColuna (l:m) = trocarPrimUlt l : trocarColuna m

acharNum :: Eq a => [a] -> a ->  Int
acharNum [] _ = -1
acharNum (h:t) n = acharNumAux (h:t) n 0 
 
acharNumAux :: Eq a => [a] -> a -> Int -> Int
acharNumAux [] _ _ = -1
acharNumAux (h:t) i m 
 | h == i = m
 | h \= i = acharNumAux t i (m+1)

posicaoMatriz :: [[Int]] -> Int -> (Int,Int)
posicaoMatriz [] _ = (-1,-1)
posicaoMatriz m y = posicaoMatrizAux m y 0

posicaoMatrizAux :: [[Int]] -> Int -> Int -> Int
posicaoMatrizAux [] _ _ = (-1,-1)
posicaoMatrizAux (h:t) y i 
 | elem y h = (acharNum h y , i)
 | otherwise = posicaoMatrizAux t y (i+1) 

substituiLista :: Eq a => [a] -> a -> Int -> [a]
substituiLista [] _ _ = []
substituiLista (x:xs) y 0 = y:xs
substituiLista (x:xs) y p = x : substituiLista xs y (p-1)

substituiMatriz :: [[Int]] -> Int -> (Int,Int) -> [[Int]] 
substituiMatriz [] _ _ = []
substituiMatriz (l:m) z (x,y)
 | y == 0 = (substituiLista l z x) : m 
 | otherwise = l : substituiMatriz m z (x,y+1)
 


