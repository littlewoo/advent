
import Data.Bits
import qualified Text.Regex as Regex
import qualified Data.Map as Map
import System.Environment

main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (let prog = map parseInstruction $ lines text in (part1 prog, part2 prog))

maskRegex = Regex.mkRegex "mask = ([01X]{36})"
memRegex  = Regex.mkRegex "mem\\[([0-9]*)\\] = ([0-9]*)"

data Instruction = Mask Mask | Mem Address Value deriving Show
type State      = (Mask, Memory)
type Memory     = Map.Map Address Value
type Program    = [Instruction]
type Mask       = [Char]
type Address    = Int
type Value      = Int
type RunInst    = Instruction -> State -> State

part1 :: Program -> Int
part1 p = runProgram applyInst1 p (replicate 36 'X', Map.empty)

part2 :: Program -> Int
part2 p = runProgram applyInst2 p (replicate 36 '1', Map.empty)

runProgram :: RunInst -> Program -> State -> Int
runProgram _ []           (_   , mem) = foldl (+) 0 mem
runProgram f (inst:insts) (mask, mem) = runProgram f insts $ f inst (mask, mem)

applyInst1 :: RunInst
applyInst1 (Mask m1) (_, mem)    = (m1, mem)
applyInst1 (Mem a v) (mask, mem) = (mask, Map.insert a (maskValue mask v) mem)

applyInst2 :: RunInst
applyInst2 (Mask m1) (_, mem)    = (m1, mem)
applyInst2 (Mem a v) (mask, mem) = (mask, insertAll (map (\addr -> (addr,v)) (maskAddr mask a)) mem)

insertAll :: Ord k => [(k,v)] -> Map.Map k v -> Map.Map k v
insertAll []          m = m
insertAll ((k,v):kvs) m = insertAll kvs $ Map.insert k v m

parseInstruction :: String -> Instruction
parseInstruction s = let maskMatch = Regex.matchRegex maskRegex s in
                        case maskMatch of 
                            Just mm -> Mask (mm !! 0)
                            Nothing -> let (Just mm) = Regex.matchRegex memRegex s in 
                                          Mem (parseInt $ mm !! 0) (parseInt $ mm !! 1)                                        

parseInt :: String -> Int
parseInt = (read :: String -> Int)

applyMem :: Mask -> Value -> Address -> Memory -> Memory
applyMem mask v addr mem = Map.insert addr (maskValue mask v) mem

maskValue :: Mask -> Value -> Value
maskValue []     v = v
maskValue (c:cs) v = let b = (2^(length cs)) in 
                            (case c of 
                               'X' -> (v .&. b)  
                               '1' ->  b
                               '0' ->  0) + maskValue cs (v .&. (b-1))



maskAddr :: Mask -> Address -> [Address]
maskAddr []       a = [0]
maskAddr ('0':cs) a = map (\n -> n + (a .&. (2^(length cs)))) (maskAddr cs a)
maskAddr ('1':cs) a = map (\n -> n + (2^(length cs))) (maskAddr cs a)
maskAddr ('X':cs) a = let lowerBits = (maskAddr cs a) in lowerBits ++ map (\n -> n + (2^(length cs))) lowerBits


