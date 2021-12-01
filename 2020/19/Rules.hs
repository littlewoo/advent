
import qualified Data.IntMap as M
import qualified Data.List as L
import Data.List.Split

data Rule = C Char | R [RuleSequence] deriving Show
type RuleIndex = Int
type RuleSequence = [RuleIndex]
type RuleCatalogue = M.IntMap Rule
readInt = read :: String -> Int

run :: String -> IO () 
run filename = do
                 rawInput     <- readFile filename
                 let (cat, ss) = parseInput $ lines rawInput
                 print (length $ filter (evaluateRuleSequence cat [0]) ss)

evaluateRuleSequence :: RuleCatalogue -> RuleSequence -> String -> Bool
evaluateRuleSequence _    []     []    = True
evaluateRuleSequence _    rs     []    = False
evaluateRuleSequence _    []     ss    = False
evaluateRuleSequence cat (r:rs) (s:ss) = case (M.!) cat r of
                                            C c    -> c == s && evaluateRuleSequence cat rs ss
                                            R seqs -> any (\seq -> evaluateRuleSequence cat (seq++rs) (s:ss)) seqs

parseInput :: [String] -> (RuleCatalogue,[String])
parseInput ss = let (rules:strings:[]) = splitOn [""] ss in (parseRules rules, strings)

parseRules :: [String] -> RuleCatalogue
parseRules []     = M.empty
parseRules (s:ss) = let (i,r) = parseRule s in M.insert i r $ parseRules ss

parseRule :: String -> (Int, Rule)
parseRule s = let (i:rem:[]) = splitOn ":" s in
                if elem '\"' rem 
                    then (readInt i,C (((read :: String -> String) rem) !! 0))
                    else (readInt i,R (parseSequences rem))

parseSequences :: String -> [RuleSequence]
parseSequences s = map (map readInt . splitOn " " . strip) $ splitOn "|" s 

strip :: String -> String
strip = dropWhile (==' ') . L.dropWhileEnd (==' ')
