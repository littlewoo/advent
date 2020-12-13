

import System.Environment
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
        args <- getArgs
        text <- readFile $ args !! 0
        print (let r = readGrid $ lines $ text in (part1 r, part2 r))       

part1 :: GenerationResult -> Int
part1 gr = (\(Stable n) -> n) (generate getPart1Neighbours (birthRules1, surviveRules1) gr)

part2 :: GenerationResult -> Int
part2 gr = (\(Stable n) -> n) (generate getPart2Neighbours (birthRules2, surviveRules2) gr)

birthRules1   = [True,  False, False, False, False, False, False, False, False]
surviveRules1 = [True,  True,  True,  True,  False, False, False, False, False]
neighbours    = [((-1), (-1)), ((-1),(0)), ((-1),(1)), ((0),(-1)), ((0),(1)), ((1),(-1)), ((1),(0)), ((1),(1))]
birthRules2   = [True,  False, False, False, False, False, False, False, False]
surviveRules2 = [True,  True,  True,  True,  True,  False, False, False, False]

data Cell = Empty | Seat Bool
data GenerationResult = Changes [[Cell]] | Stable Int
type FindNeighbours = (Int,Int) -> GenerationResult -> [(Int,Int)]
type Ruleset = ([Bool], [Bool])

instance Show Cell where
    show Empty        = "."
    show (Seat True)  = "#"
    show (Seat False) = "L"

instance Show GenerationResult where
    show (Changes cells) = concat $ map showRow cells
    show (Stable  n)     = show n

instance Eq GenerationResult where 
    (==) a b = (show a) == (show b)

readGrid :: [String] -> GenerationResult
readGrid ss = Changes $ map readRow ss

readRow :: String -> [Cell]
readRow []     = []
readRow (c:cs) = ((case c of 
                    '.' -> Empty
                    '#' -> Seat True
                    'L' -> Seat False) : readRow cs)

showRow :: [Cell] -> String
showRow r = concat $ (map (\l -> show l) r) ++ ["\n"]

generate :: FindNeighbours -> Ruleset -> GenerationResult -> GenerationResult
generate fn rules (Changes gr) = let new = (Changes $ map (\r -> generateRow fn rules (0, r) (Changes gr) (getRow r (Changes gr))) [0..((height gr)-1)]) in if new == (Changes gr) then Stable $ countOccupied new else generate fn rules new

countOccupied :: GenerationResult -> Int
countOccupied (Changes gr) = length $ concat $ map (filter (\r -> isOccupied (Just r))) gr

generateRow :: FindNeighbours -> Ruleset -> (Int, Int) -> GenerationResult -> [Cell] -> [Cell]
generateRow _  _     _     _    []     = []
generateRow fn rules (x,y) prev (l:ls) = ((generateCell rules (getNumberOfLiveNeighbours fn (x,y) prev) l): generateRow fn rules (x+1,y) prev ls)

generateCell :: Ruleset -> Int -> Cell -> Cell
generateCell (birthRules, surviveRules) _          Empty    = Empty
generateCell (birthRules, surviveRules) neighbours (Seat c) = Seat ((c      && (surviveRules !! neighbours)) || 
                                                                   ((not c) && (birthRules   !! neighbours))) 
                                            

getNumberOfLiveNeighbours :: FindNeighbours -> (Int, Int) -> GenerationResult -> Int
getNumberOfLiveNeighbours fn (x,y) r = length $ filter isOccupied $ map (get r) $ fn (x,y) r

isOccupied :: Maybe Cell -> Bool
isOccupied Nothing         = False
isOccupied (Just Empty)    = False
isOccupied (Just (Seat b)) = b

get :: GenerationResult -> (Int, Int) -> Maybe Cell
get (Changes gr) (x, y) = if (x < 0) || (y < 0) || (x >= (width gr)) || (y >= height gr) then Nothing 
                            else Just ((gr !! y) !! x)

height :: [[a]] -> Int
height rs = length rs

width :: [[a]] -> Int
width rs  = length (rs !! 0)

getRow :: Int -> GenerationResult -> [Cell]
getRow n (Changes rs) = rs !! n

getPart1Neighbours :: FindNeighbours
getPart1Neighbours (x,y) _ = map (\(a,b) -> (a+x, b+y)) neighbours

findNextNeighbourInDirection :: (Int, Int) -> (Int, Int) -> GenerationResult -> Maybe (Int, Int)
findNextNeighbourInDirection (x,y) (a,b) gr = let c = get gr ((x+a),(y+b)) in (case c of 
                                                        Nothing       -> Nothing 
                                                        (Just Empty)  -> findNextNeighbourInDirection ((x+a),(y+b)) (a,b) gr
                                                        Just (Seat s) -> Just ((x+a),(y+b))) 

getPart2Neighbours :: FindNeighbours
getPart2Neighbours (x,y) gr = map (Maybe.fromJust) $ filter (Maybe.isJust) $ map (\(a,b) -> findNextNeighbourInDirection (x,y) (a,b) gr) neighbours

