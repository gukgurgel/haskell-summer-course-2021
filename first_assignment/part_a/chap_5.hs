-- Problem 5.9

data ListaOrd a = a :?: (ListaOrd a) | Nulo deriving (Show)

inserir :: (Ord a) => a -> ListaOrd a -> ListaOrd a
inserir x Nulo = x :?: Nulo
inserir x (a :?: resto)
  | a > x = x :?: (a :?: resto)
  | otherwise = a :?: inserir x resto

remover :: (Eq a) => a -> ListaOrd a -> ListaOrd a
remover x Nulo = Nulo
remover x (a :?: resto)
  | a == x = resto
  | otherwise = a :?: remover x resto

tamanho :: ListaOrd a -> Int
tamanho Nulo = 0
tamanho (a :?: resto) = 1 + tamanho resto

-- exemple:
-- *Main> inserir 3 (4 :?: (5 :?: (7 :?: Nulo)))
-- 3 :?: (4 :?: (5 :?: (7 :?: Nulo)))

-- Problem 5.10

data Tree a = NuloTree |
              Leaf a |
              Branch a (Tree a) (Tree a) deriving Show

sumTree :: Tree Double -> Double
sumTree (Branch x l r) = sumTree l + x + sumTree r
sumTree (Leaf x) = x
sumTree NuloTree = 0

-- *Main> sumTree (Branch 50 (Branch 30 (Leaf 20) (Leaf 40)) (Branch 90 NuloTree (Leaf 100)))
-- 330
