import Data.List
import Control.Arrow ((>>>))

base :: Integer
base = 10

digitSum :: Integer -> Integer
digitSum n = aux n 0
    where aux n acc
            | n < base = n + acc
            | otherwise = aux (n `div` base) (acc + n `mod` base)

superDigit :: Integer -> Integer
superDigit n
  | n < base = n
  | otherwise = superDigit $ digitSum n

solve :: [Integer] -> Integer
solve [n, k] = superDigit $ k * digitSum n

main :: IO ()
main = interact $
    words >>> map read >>> solve >>> show
