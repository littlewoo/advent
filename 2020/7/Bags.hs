
import System.Environment
import qualified Text.Regex as Regex
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

main :: IO ()
main = do
            args <- getArgs
            text <- readFile (args !! 0)
            print (let rs = parseRules (lines text) in (part1 rs, part2 rs))

type Colour = String
type BagRule = (Colour, [(Int, Colour)])

bagColour = Regex.mkRegex "([a-z]+ [a-z]+) bag[s]? contain"
contents = Regex.mkRegex " ([0-9]) ([a-z]+ [a-z]+) bag[s]?[,.]"

part1 :: [BagRule] -> Int
part1 rs = Set.size $ canContainTarget "shiny gold" rs 

part2 :: [BagRule] -> Int 
part2 rs = (mustContain "shiny gold" (Map.fromList rs)) -1

parseRules :: [String] -> [BagRule]
parseRules = map parseRule

parseRule :: String -> BagRule
parseRule s = let (_,_,r,(m:[])) = fromJust $ Regex.matchRegexAll bagColour s in (m, parseContents r)

parseContents :: String -> [(Int, Colour)]
parseContents s = case (Regex.matchRegexAll contents s) of
                        Nothing         -> []
                        Just (_,_,r,ms) -> (parseContentElement ms : parseContents r) 
                            
parseContentElement :: [String] -> (Int, Colour)
parseContentElement (qty:col:[]) = ((read :: String -> Int) qty,  col)

canContainTarget :: Colour -> [BagRule] -> Set.Set Colour
canContainTarget c rs = let possParents = allColoursContainingColour c rs in Set.union possParents $ Set.unions $ Set.map (\col -> canContainTarget col rs) possParents

allColoursContainingColour :: Colour -> [BagRule] -> Set.Set Colour
allColoursContainingColour c rs = Set.fromList $ map fst $ filter (ruleContainsColour c) rs

ruleContainsColour :: Colour -> BagRule -> Bool
ruleContainsColour col (_,children) = length (filter (\(_,c) -> col == c) children) > 0

mustContain :: Colour -> Map.Map Colour [(Int, Colour)] -> Int
mustContain col rs = (foldr (+) 1 (map (\(n, c) -> n * (mustContain c rs)) ((Map.!) rs col)))
