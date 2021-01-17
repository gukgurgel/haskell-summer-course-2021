import Data.List
import Control.Arrow ((>>>))

readLine :: String -> Int
readLine x = read x

execute :: [Int] -> Int

execute list = num_three * num_one
  where num_one = find_answer (sort (0 : list)) 1
        num_three = (find_answer (sort (0 : list)) 3) + 1 -- + 1 comes from the cellphone

find_answer :: [Int] -> Int -> Int

find_answer list n = (length . filter (== n)) diff_list
  where diff_list = zipWith (-) (tail list) (init list)

main :: IO ()
main = interact $
    lines >>> map readLine >>> execute >>> show
