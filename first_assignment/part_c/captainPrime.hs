import Control.Arrow ((>>>))

data Fate = CENTRAL | LEFT | RIGHT | DEAD deriving Show

isqrt :: Int -> Int
isqrt n = aux 1
    where aux r
            | r * r > n = r - 1
            | otherwise = aux (r + 1)

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = not $ any (\r -> n `mod` r == 0) [2..isqrt n]

base :: Int
base = 10

digits :: Int -> [Int]
digits n = aux n []
    where aux n acc
            | n < base = n : acc
            | otherwise = aux (n `div` base) (n `mod` base : acc)

undigits :: [Int] -> Int
undigits ds = aux ds 0
    where aux [] acc = acc
          aux (d : ds) acc = aux ds (d + 10 * acc)

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = xs : suffixes (tail xs)

prefixes :: [a] -> [[a]]
prefixes = map reverse . suffixes . reverse

getFate :: Int -> Fate
getFate idNum
  | not (isPrime idNum) || 0 `elem` digits idNum = DEAD
  | left && right = CENTRAL
  | left = LEFT
  | right = RIGHT
  | otherwise = DEAD
  where left = all (isPrime . undigits) (suffixes $ digits idNum)
        right = all (isPrime . undigits) (prefixes $ digits idNum)

main :: IO ()
main = interact $
    lines >>> tail >>> map (read >>> getFate >>> show) >>> unlines
