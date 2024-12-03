
import System.Environment
import qualified Data.List as List
import qualified Data.Map as Map

main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (puzzle1 text)
            print (puzzle2 text)

puzzle1 :: String -> Int
puzzle1 input = let vals = map List.sort $ List.transpose $ parseInput input in
                    sum $ distances (vals !! 0) (vals !! 1)

parseInput :: String -> [[Int]]
parseInput s = map parseLine $ lines s

parseLine :: String -> [Int]
parseLine s = map (read :: String -> Int) $ words s

distance :: Int -> Int -> Int
distance x y = abs $ x - y 

distances :: [Int] -> [Int] -> [Int]
distances [] []         = []
distances (x:xs) (y:ys) = (distance x y : distances xs ys)

puzzle2 :: String -> Int
puzzle2 input = let sortedLists = List.map List.sort $ List.transpose $ parseInput input in
                    similarity (sortedLists !! 0) $ Map.fromList (counts (sortedLists !! 1))

similarity :: [Int] -> (Map.Map Int Int) -> Int
similarity []     _     = 0
similarity (x:xs) cs    = x * (orZero (Map.lookup x cs)) + similarity xs cs

orZero :: Maybe Int -> Int
orZero Nothing   = 0
orZero (Just n)  = n

counts :: [Int] -> [(Int, Int)]
counts []     = []
counts (x:[]) = [(x, 1)]
counts (x:xs) = let (ex,next) = span (==x) xs in ((x, 1 + length ex):counts next)
