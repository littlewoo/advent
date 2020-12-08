
import System.Environment
import qualified Text.Regex as Regex
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

main :: IO ()
main = do
        args <- getArgs
        text <- readFile (args !! 0)
        print (let p = (parseProgram $ lines text) in (part1 p, part2 p))


type ComputerState = (PC, Acc, Visited)
type PC = Int
type Acc = Int
data Instruction = Jmp | Acc | Nop deriving Show
type Program = [(Instruction, Int)]
type Visited = Set.Set Int

instructionRegex = Regex.mkRegex "(jmp|acc|nop) ([+-])([0-9]*)"

parseProgram :: [String] -> Program
parseProgram = map (\s -> let (instr : sign : qty : []) = fromJust $ Regex.matchRegex instructionRegex s in 
                                (case instr of 
                                    "jmp" -> Jmp
                                    "acc" -> Acc
                                    "nop" -> Nop,
                                 case sign of 
                                    "+"   -> (read :: String -> Int) qty
                                    "-"   -> (- (read :: String -> Int) qty)))

part1 :: Program -> Int
part1 prog = snd $ execute prog (0,0,Set.empty)

part2 :: Program -> Int
part2 prog = let results = executeAllPermutations prog in snd ((filter (\(r,_) -> not r) results) !! 0)

executeAllPermutations :: Program -> [(Bool, Acc)]
executeAllPermutations prog = map (\p -> execute p (0,0,Set.empty)) (allPermutations prog)

execute :: Program -> ComputerState -> (Bool, Acc)
execute prog (pc, acc, vis) = if (Set.member pc vis) then (True, acc) else 
                                    if (pc >= (length prog)) then (False, acc) else
                                        execute prog $ executeInstr (prog !! pc) (pc, acc, vis) 
                             

executeInstr :: (Instruction, Int) -> ComputerState -> ComputerState
executeInstr (instr, qty) state = (case instr of 
                                        Jmp -> exeJmp
                                        Acc -> exeAcc
                                        Nop -> exeNop) state qty

exeJmp :: ComputerState -> Int -> ComputerState
exeJmp (pc, acc, vis) qty = (pc + qty, acc, Set.insert pc vis) 

exeAcc :: ComputerState -> Int -> ComputerState
exeAcc (pc, acc, vis) qty = (pc + 1, acc + qty, Set.insert pc vis)

exeNop :: ComputerState -> Int -> ComputerState
exeNop (pc, acc, vis) qty = (pc + 1, acc, Set.insert pc vis)

allPermutations :: Program -> [Program]
allPermutations p = map fromJust $ filter (isJust) $ map (permuteProgram p) [0..((length p)-1)]

permuteProgram :: Program -> Int -> Maybe Program
permuteProgram prog n = let (pre,((instr,qty):post)) = splitAt n prog in 
                            case instr of 
                                Jmp -> Just (pre ++ ((Nop,qty):post))
                                Nop -> Just (pre ++ ((Jmp,qty):post))
                                Acc -> Nothing           
