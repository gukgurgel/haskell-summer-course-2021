import Data.List
import Text.Regex.TDFA

data Instruction = Acc Int | Jmp Int | Nop Int deriving Show
type Program = [Instruction]
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

execute :: Program -> Maybe Acc
execute program = aux 0 [] 0
    where aux line visited acc
            | line `elem` visited = Nothing
            | line == length program = Just acc
            | otherwise = aux (nextLine line op) (line : visited) (nextAcc acc op)
            where op = program !! line

switchOp :: Instruction -> Instruction
switchOp (Jmp x) = Nop x
switchOp (Nop x) = Jmp x
switchOp op = op

corrupt :: Program -> Line -> Program
corrupt program line = begin ++ [switchOp op] ++ end
    where (begin, op : end) = splitAt (line - 1) program

corruptions :: Program -> [Program]
corruptions program = [corrupt program line | line <- [0..length program]]

findSol :: [Maybe Acc] -> Acc
findSol (Just acc : _) = acc
findSol (_ : xs) = findSol xs

main :: IO ()
main = interact $ show . findSol . map execute . corruptions . map readInstruction . lines
