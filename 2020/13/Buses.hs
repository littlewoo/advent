
import System.Environment
import Data.List.Split

main :: IO ()   
main = do
         args <- getArgs
         text <- readFile (args !! 0)
         print ((part1 $ lines text, part2 $ lines text))

part1 :: [String] -> Int
part1 ss = let (earliestDeparture, buses) = parsePart1 ss in findPart1Answer $ map (\n -> (n, n - (earliestDeparture `mod` n))) buses

findPart1Answer :: [(Int, Int)] -> Int
findPart1Answer xs = findPart1Answer' (head xs) (tail xs) 

findPart1Answer' :: (Int, Int) -> [(Int, Int)] -> Int
findPart1Answer' (mB,mT)   []         = mB*mT
findPart1Answer' (mB,mT)   ((b,t):xs) = if (t < mT) then findPart1Answer' (b,t) xs else findPart1Answer' (mB,mT) xs

parsePart1 :: [String] -> (Int, [Int])
parsePart1 ss = ((read :: String -> Int) (ss !! 0), map (read :: String -> Int) $ filter (/= "x") $ splitOn "," $ ss !! 1)

part2 :: [String] -> String
part2 _ = "lol https://www.dcode.fr/chinese-remainder - sorry" 

mapInput :: String -> [(Int, Maybe Int)]
mapInput ss = (mapWithIndices 0) $ ((map mapElem) . (splitOn ",")) ss 

mapElem :: String -> Maybe Int
mapElem s = if s == "x" then Nothing else (Just $ (read :: String -> Int) s)

mapWithIndices :: Int -> [a] -> [(Int, a)]
mapWithIndices i []     = []
mapWithIndices i (x:xs) = ((i, x):mapWithIndices (i+1) xs)

findAcceptableTimes :: [Int] -> (Int, Maybe Int) -> [Int]
findAcceptableTimes prev (_, Nothing) = prev
findAcceptableTimes prev (n, Just i)  = filter (isAcceptableTime (n, i)) prev

isAcceptableTime :: (Int, Int) -> Int -> Bool
isAcceptableTime (offset, period) time = ((time + offset) `mod` period) == 0
