-- | 2

-- | a)
-- > area 2.0
--   4.0

area :: Float -> Float
area lado = lado * lado


-- / b)
-- > perimetro 4.5 1.5
--   6.0

perimetro :: Float -> Float -> Float
perimetro comp larg = comp + larg


-- / c) 
-- > pertence 'c' "Oscar"
--   True
-- > pertence 'c' "Otavio"
--   False

pertence :: Char -> String -> Bool
pertence _ [] = False
pertence x l
  | x == head l = True
  | otherwise = pertence x (tail l)


-- / d)
-- > remove_par "Ola"
--   "Ol"
-- > remove_par "Olaf"
--   "laf"

remove_par :: String -> String
remove_par l
  | mod (length l) 2 == 0 = tail l
  |otherwise = init l


-- / e)
-- > par [5,6,7,3]
--   (5,3)
-- > par ["Haskell", "C", "Python", "Elixir"]
--   ("Haskell", "Elixir")
par :: [a] -> (a,a)
par l = (head l, last l)


-- / f)
-- EX: > nome ["Salome", "Pereira", "Faria"]
--      ("Salome","Faria")

nome :: [String] -> (String,String)
nome l = (head l, last l)


-- / g)
-- > par_listas (["Haskell", "C", "Python", "Elixir"],["Salome", "Pereira", "Faria"])
--   ("Haskell",["Salome", "Pereira", "Faria"])

par_listas :: ([a],[b]) -> (a,[b])
par_listas (xs,ys) = (head xs, ys)


-- / h)
-- > abreviatura ["Joaquim", "Francisco", "Alves", "Martins"]
--   "J.Martins"

abreviatura :: [String] -> String
abreviatura l = head(head l) : '.' : (last l)


