
import System.Environment
import Data.List.Split
import qualified Data.Set as Set

main :: IO ()
main = do
            args <- getArgs
            text <- readFile (args !! 0)
            print (part1 text, part2 text)

part1 :: String -> Int
part1 s = foldr (+) 0 $ map (Set.size . Set.fromList) $ map (filter (/= '\n')) $ splitOn "\n\n" s

part2 :: String -> ([(Set.Set Char, Int)], Int)
part2 s = let o = form (map allYeses (groups s)) in (o, foldr (+) 0 (map snd o))   


form :: [Set.Set Char] -> [(Set.Set Char, Int)]
form = map (\x -> (x, Set.size x))

groups :: String -> [[Set.Set Char]]
groups s = map (map Set.fromList) (map (filter (/= "") . splitOn "\n") (splitOn "\n\n" s))

allYeses :: [Set.Set Char] -> Set.Set Char
allYeses (x:xs) = allYeses' x xs

allYeses' :: Set.Set Char -> [Set.Set Char] -> Set.Set Char
allYeses' prev []     = prev
allYeses' prev (n:ns) = allYeses' (Set.intersection prev n) ns

