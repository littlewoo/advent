
import System.Environment
import Text.Regex 
import Data.List
import qualified Data.IntMap as M

rulesRegex = mkRegex "([a-z ]*): ([0-9]*)-([0-9]*) or ([0-9]*)-([0-9]*)"

type FieldName  = String
type Range      = (Int, Int)
data Rule       = R FieldName Range Range deriving (Show, Eq) 
data Field      = F FieldName Int deriving Show
type Ticket     = [Field]
type RawTicket  = [Int]


main :: IO ()
main = do
            args <- getArgs
            text <- readFile (args !! 0)
            let input = lines text
            print (part2 input)

part1 :: [String] -> Int
part1 ss = do 
                let (rules, myTicket, nearbyTickets) = parseInput ss
                let ranges = flattenRules rules
                let validNums = validValues ranges
                let flatTickets = concat (myTicket:nearbyTickets)
                sum $ filter (not . atLeastOneInRange ranges) flatTickets                

part2 :: [String] -> Ticket
part2 ss = do
                let (rules, myTicket, nearbyTickets) = parseInput ss
                let ranges = flattenRules rules
                let validTickets = validateTickets (myTicket: nearbyTickets) (validValues ranges)
                let allRulePossibilities = map (mapToPossibleRules rules) (myTicket:validTickets)
                let allPossibleRulesByColumn = matchFields allRulePossibilities
                let actualColumns = reduceByElimination allPossibleRulesByColumn M.empty
                interpretTicket myTicket actualColumns

interpretTicket :: RawTicket -> M.IntMap FieldName -> Ticket
interpretTicket = interpretTicket' 0 

interpretTicket' :: Int -> RawTicket -> M.IntMap FieldName -> Ticket
interpretTicket' _ []     _      = []
interpretTicket' n (f:fs) fields = (F ((M.!) fields n) f:interpretTicket' (n+1) fs fields)

flattenRules :: [Rule] -> [Range]
flattenRules []             = []
flattenRules (R _ r1 r2:rs) = (r1:r2:flattenRules rs) 

parseInput :: [String] -> ([Rule], RawTicket, [RawTicket])
parseInput ss = let ts = parseTickets ss in (parseRules ss, fst ts, snd ts)

parseRules :: [String] -> [Rule]
parseRules []     = []
parseRules (s:ss) = let rules = parseRules ss 
                        mm    = matchRegex rulesRegex s in  
                                    case mm of 
                                        Just m -> (parseRule m : rules)
                                        Nothing -> rules

-- find my ticket, then all the rest
parseTickets :: [String] -> (RawTicket, [RawTicket])
parseTickets ss = do
                    let fromMine = dropWhile (/= "your ticket:") ss
                    let myTicket = parseTicket $ head $ tail fromMine   
                    let fromNear = tail $ dropWhile (/= "nearby tickets:") fromMine
                    (myTicket, map parseTicket fromNear)
                    

parseRule :: [String] -> Rule
parseRule []      = R "" (0,0) (0,0)
parseRule matches = let r = (read :: String -> Int) in 
                        R (matches !! 0) ((r $ matches !! 1), (r $ matches !! 2)) ((r $ matches !! 3),(r $ matches !! 4))

parseTicket :: String -> RawTicket
parseTicket s = (read :: String -> [Int]) $ "[" ++ s ++ "]"

isInRange :: Range -> Int -> Bool
isInRange (x,y) n = (x <= n) && (n <= y)

atLeastOneInRange :: [Range] -> Int -> Bool
atLeastOneInRange [] _     = False
atLeastOneInRange (r:rs) n = (isInRange r n) || atLeastOneInRange rs n

matchesRule :: Rule -> Int -> Bool
matchesRule (R _ r1 r2) n = isInRange r1 n || isInRange r2 n

validateTickets :: [RawTicket] -> [Int] -> [RawTicket]
validateTickets ts valid = filter (\t -> all (`elem` valid) t) ts 

validValues :: [Range] -> [Int]
validValues rs = filter (atLeastOneInRange rs) [0..1000]

mapToPossibleRules :: [Rule] -> RawTicket -> [[Rule]]
mapToPossibleRules _   []    = []
mapToPossibleRules rs (n:ns) = ((filter (\r -> matchesRule r n) rs) : mapToPossibleRules rs ns)

-- in a list of lists of possible rules, find the set of rules which can be in pos 1, pos 2, etc.
matchFields :: [[[Rule]]] -> [[Rule]]
matchFields []               = []
matchFields ([]:_)           = []  
matchFields possibleRuleSets = (intersection (map head possibleRuleSets):matchFields (map tail possibleRuleSets))

intersection :: Eq a => [[a]] -> [a]
intersection []     = []
intersection (s:[]) = s
intersection (s:ss) = filter (`elem` s) $ intersection ss

-- reduce a list of lists by taking any singletons as elements, and removing that element from the rest of the lists
--   note that this makes some big assumptions about the validity of the input
reduceByElimination :: [[Rule]] -> M.IntMap FieldName -> M.IntMap FieldName
reduceByElimination unreduced reduced = do
                                    let (Just firstSingletonIndex) = findIndex (\xs -> length xs == 1) unreduced
                                    let singletonElem = (unreduced !! firstSingletonIndex) !! 0
                                    let remainingUnreduced = map (delete singletonElem) unreduced
                                    let reducedSoFar = M.insert firstSingletonIndex ((\(R fn _ _) -> fn) singletonElem) reduced
                                    if M.size reduced == length unreduced 
                                        then reduced
                                        else reduceByElimination remainingUnreduced reducedSoFar
                                             


replaceElemAt :: Int -> a -> [a] -> [a]
replaceElemAt i x xs = (take i xs) ++ [x] ++ (drop (i+1) xs)
