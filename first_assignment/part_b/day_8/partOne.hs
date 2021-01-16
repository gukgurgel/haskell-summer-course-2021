---- Pragmas ----
{-# LANGUAGE ViewPatterns #-}

---- Imports ----
import Data.List
import Control.Arrow ((>>>))

---- Types ----
data Instruction = Acc Int | Jmp Int | Nop Int deriving Show

---- Type synonyms ----
type Line = Int
type Acc = Int

---- Parsing functions -----
readSigint :: String -> Acc
readSigint ('+' : s) = read s
readSigint s = read s

readInstruction :: String -> Instruction
readInstruction (stripPrefix "acc " -> Just s) = Acc (readSigint s)
readInstruction (stripPrefix "jmp " -> Just s) = Jmp (readSigint s)
readInstruction (stripPrefix "nop " -> Just s) = Nop (readSigint s)
readInstruction _ = error "invalid instruction"

---- Solution of Part One ----
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
main = interact $
    lines >>> map readInstruction >>> execute >>> show
