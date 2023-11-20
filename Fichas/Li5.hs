zip' :: Num b => [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' (h:t) [] = (h,0) : zip' t []
zip' (h:t) (x:y) = (h,x) : zip' t y

zipSec :: [Int] -> [Int] -> [(Int,Int)]
zipSec l1 l2 
 | n1 == n2 = zip l1 l2
 | n1 < n2 = zip (l1 ++ [0,0..]) l2
 | n1 > n2 = zip l1 (l2 ++ [0,0..])
 where n1 = length l1
       n2 = length l2

contaPalavra :: String -> String -> Int
contaPalavra t p =
    let lp = words t
    in contaP lp p 
     where contaP :: [String] -> String -> Int
           contaP [] _ = 0
           contaP (p1:lp) p
            | p == p1 = 1 + contaP lp p 
            | otherwise = contaP lp p

f :: [(String,Int)] -> [(String,Int)] -> [(String,Int)]
f l [] = l 
f [] l = l 
f (h:t) ((x,z):y)
 | x == fst h = (x,z) : faux (h:t) y
 | otherwise = faux (h:t) y : f (h:t) 

faux :: (String,Int) -> [(String,Int)] -> (String,Int)
faux (x,z) [] = (x,y)
faux 