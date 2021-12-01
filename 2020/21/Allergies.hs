
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import Data.List.Split
import Text.Regex

data Ingredient = I String deriving (Ord, Eq, Show) 
data Allergen = A String deriving (Ord, Eq, Show)
data IngredientAllergen = Confirmed Ingredient (Maybe Allergen)

allergensRegex = mkRegex ".*\\(contains (.*)\\)"

part1 :: String -> IO ()
part1 filename = do
                    rawInput <- readFile filename
                    let foods = parseInput rawInput
                    let allergens = setOfAllergens foods
                    let candidates = M.fromList . S.toList $ S.map (\a -> (a, findAllergenCandidates a foods)) allergens
                    let ingredientAllergens = reduceByElimination candidates M.empty
                    let allergenFreeIngredients = S.difference (setOfIngredients foods) (M.keysSet ingredientAllergens)
                    print allergenFreeIngredients
                    let instances = sum $ map (\i -> countInstances i foods) $ S.toList allergenFreeIngredients
                    print instances

part2 :: String -> IO ()
part2 filename = do
                    rawInput <- readFile filename
                    let foods = parseInput rawInput
                    let allergens = setOfAllergens foods
                    let candidates = M.fromList . S.toList $ S.map (\a -> (a, findAllergenCandidates a foods)) allergens
                    let ingredientAllergens = reduceByElimination candidates M.empty
                    print (showDangerList ingredientAllergens)

showDangerList :: M.Map Ingredient Allergen -> String
showDangerList is = do
                        let sorted = L.sortOn (\(k,v) -> v) $ M.toList is
                        foldr (\((I i),a) s -> i ++ "," ++ s) "" sorted 
                        

countInstances :: Ingredient -> [([Ingredient],[Allergen])] -> Int
countInstances _ [] = 0
countInstances i ((is,_):next) = (countInstances i next) + (length $ filter (==i) is)

findAllergenCandidates :: Allergen -> [([Ingredient],[Allergen])] -> S.Set Ingredient
findAllergenCandidates a entries = let candidates = filter (\(_, as) -> a `elem` as) entries in
                                        intersections (map (\(ings, _) -> S.fromList ings) candidates) 

reduceByElimination :: M.Map Allergen (S.Set Ingredient) -> M.Map Ingredient Allergen -> M.Map Ingredient Allergen
reduceByElimination unreduced reduced = if M.null unreduced then reduced else do
                                            let (a,i) = findASingleton (M.toList unreduced)
                                            let remainingUnreduced = removeAllergenCandidate (M.toList (M.delete a unreduced)) i
                                            let reducedSoFar = M.insert i a reduced
                                            reduceByElimination remainingUnreduced reducedSoFar

removeAllergenCandidate :: [(Allergen, S.Set Ingredient)] -> Ingredient -> M.Map Allergen (S.Set Ingredient)
removeAllergenCandidate []              _ = M.empty
removeAllergenCandidate ((a,ings):next) i = M.insert a (S.delete i ings) $ removeAllergenCandidate next i

findASingleton :: [(Allergen, (S.Set Ingredient))] -> (Allergen, Ingredient)
findASingleton ((a, ings): next) = if S.size ings == 1 then (a, (S.elemAt 0 ings)) else findASingleton next

intersections :: Ord a => [S.Set a] -> S.Set a
intersections []        = S.empty
intersections (s:[])    = s
intersections (s:ss)    = S.intersection s $ intersections ss

parseInput :: String -> [([Ingredient],[Allergen])]
parseInput input = map (\s -> (extractIngredients s, extractAllergens s)) (lines input)

setOfAllergens :: [([Ingredient],[Allergen])] -> S.Set Allergen
setOfAllergens []            = S.empty
setOfAllergens ((_,as):next) = S.union (S.fromList as) (setOfAllergens next)

setOfIngredients :: [([Ingredient],[Allergen])] -> S.Set Ingredient
setOfIngredients []            = S.empty
setOfIngredients ((is,_):next) = S.union (S.fromList is) (setOfIngredients next)

parseLine :: String -> ([Ingredient], [Allergen])
parseLine s = (extractIngredients s, extractAllergens s) 

extractIngredients :: String -> [Ingredient]
extractIngredients s = map (\i -> I i) $ init $ splitOn " " $ takeWhile (\c -> not (c == '(')) s

extractAllergens :: String -> [Allergen]
extractAllergens s = case (matchRegex allergensRegex s) of 
                        Nothing -> []
                        Just m  -> map (\a -> A a) (splitOn ", " (m !! 0))
