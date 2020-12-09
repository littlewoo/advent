
import System.Environment
import Data.Maybe 

main :: IO ()
main = do
            args <- getArgs
            text <- readFile (args !! 0)
            print (let input        = (map (read :: String -> Int) (lines text)) 
                       preambleSize = ((read :: String -> Int) (args !! 1)) in 
                            (part1 input preambleSize, part2 input preambleSize))
                
            

part1 :: [Int] -> Int -> Int
part1 xs l = check (permute xs l)

part2 :: [Int] -> Int -> Int
part2 xs l = let rs = fromJust (findSublistSummingTo (part1 xs l) xs) in maximum rs + minimum rs 

check :: [([(Int, Int)], Int)] -> Int
check ((pairs, n):xs) = if doesSum (pairs,n) then check xs else n

doesSum :: ([(Int,Int)], Int) -> Bool
doesSum (pairs, n) = length (filter (\(x,y) -> x+y == n) pairs) > 0

allPossiblePairs :: [a] -> [(a,a)]
allPossiblePairs (x:y:[]) = [(x,y)]
allPossiblePairs (x:xs) = (map (\y -> (x,y)) xs) ++ allPossiblePairs xs

permute :: [Int] -> Int -> [([(Int, Int)], Int)]
permute xs l = map (\n -> slice n (n+l) xs) [0..(length xs-l-1)]

slice :: Int -> Int -> [a] -> ([(a,a)], a)
slice x y ns = (allPossiblePairs $ take (y-x) (drop x ns), ns !! y)

findSublistSummingTo :: Int -> [Int] -> Maybe [Int]
findSublistSummingTo _ []     = Nothing
findSublistSummingTo n (x:xs) = case (findSublistSummingTo' 0 [] (x:xs) n) of
                                Nothing -> findSublistSummingTo n xs
                                Just v  -> Just v

findSublistSummingTo' :: Int -> [Int] -> [Int] -> Int -> Maybe [Int]
findSublistSummingTo' _   _    []     _ = Nothing
findSublistSummingTo' acc prev (x:xs) n = let v = (acc+x) in 
                                            if v<n then findSublistSummingTo' (acc+x) (x:prev) xs n else 
                                              if v>n then Nothing else
                                                Just (x:prev) 

