
import System.Environment
import Data.List.Split

main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (puzzle1 text)
            print (puzzle2 text)

puzzle1 :: String -> Int
puzzle1 text = countMatching isContained $ map toPairPair $ lines text

puzzle2 :: String -> Int
puzzle2 text = countMatching isOverlapping $ map toPairPair $ lines text 


toPair :: String -> (Int, Int)
toPair s = let (x:y:[]) = splitOn "-" s in ((read :: String -> Int) x, (read :: String -> Int) y)

toPairPair :: String -> ((Int, Int),(Int, Int))
toPairPair s = let (x:y:[]) = splitOn "," s in (toPair x,toPair y)

isContained :: ((Int, Int),(Int,Int)) -> Bool
isContained ((x,x'),(y,y')) = (x <= y && x' >= y') || (y <= x && y' >= x')

isOverlapping :: ((Int, Int),(Int,Int)) -> Bool
isOverlapping ((x,x'),(y,y')) = between x (y,y') || between x' (y,y') || between y (x,x') || between y' (x,x')

countMatching :: (a -> Bool) -> [a] -> Int
countMatching f bs = length $ filter f bs

between :: Int -> (Int, Int) -> Bool
between x (y,y') = x >= y && x <= y'
