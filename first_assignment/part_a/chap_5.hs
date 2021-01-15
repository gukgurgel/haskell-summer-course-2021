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
