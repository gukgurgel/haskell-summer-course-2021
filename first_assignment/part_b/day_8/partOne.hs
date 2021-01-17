import Data.List
import Text.Regex.TDFA

data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
type Line = Int
type Acc = Int

readInstruction :: String -> Instruction
readInstruction s =
    let op = concat $ concat (s =~ "[acc|jmp|nop]" :: [[String]])
        n  = read $ head $ head (s =~ "-?[0-9]+" :: [[String]])
    in case op of
         "acc" -> Acc n
         "jmp" -> Jmp n
         "nop" -> Nop n
         _     -> error "invalid instruction"

nextLine :: Line -> Instruction -> Line
nextLine line (Jmp delta) = line + delta
nextLine line _ = line + 1

nextAcc :: Acc -> Instruction -> Acc
nextAcc acc (Acc delta) = acc + delta
nextAcc acc _ = acc

execute :: [Instruction] -> Acc
execute program = aux 0 [] 0
    where aux line visited acc
            | line `elem` visited = acc
            | otherwise = aux (nextLine line op) (line : visited) (nextAcc acc op)
            where op = program !! line

main :: IO ()
main = interact $ show . execute . map readInstruction . lines
