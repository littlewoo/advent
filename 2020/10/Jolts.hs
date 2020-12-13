
import System.Environment
import qualified Data.Set as Set

main :: IO ()
main = do
         args <- getArgs
         text <- readFile (args !! 0)
         print (let xs = (0:(quicksort $ map (read :: String -> Int) $ lines text)) in (onesAndThrees (0,0) xs, part2 xs))

onesAndThrees :: (Int, Int) -> [Int] -> (Int, Int)
onesAndThrees (ones, threes) (x:[])   = (ones, threes+1)                
onesAndThrees (ones, threes) (x:y:xs) = onesAndThrees (if y-x == 3 
                                                        then (ones, (threes + 1))  
                                                        else ((ones + 1), threes)) (y:xs)  
                                    

quicksort :: [Int] -> [Int]
quicksort []     = []
quicksort (p:xs) = (quicksort $ filter (<p) xs) ++ [p] ++ (quicksort $ filter (>=p) xs) 


part2 :: [Int] -> Int
part2 xs = mapToForwardPaths (pathsFromHereForward xs) !! 0
                        
mapToForwardPaths :: [[Int]] -> [Int]
mapToForwardPaths []     = [1]
mapToForwardPaths (p:ps) = let f = mapToForwardPaths ps in 
                                (sum (map (\i -> f !! i) p) : f)  

-- if we are at w, return a list of indices that can be the next node after this
pathsFromHereForward :: [Int] -> [[Int]]
pathsFromHereForward (w:[])       = [[]]
pathsFromHereForward (w:x:[])     = [[0]]
pathsFromHereForward (w:x:y:[])   = if (y-w) > 3 then [[0],[0]] else [[0,1],[0]]
pathsFromHereForward (w:x:y:z:xs) = ((if (y-w) > 3 then [0] else if (z-w) > 3 then [0,1] else [0,1,2]):pathsFromHereForward (x:y:z:xs))


