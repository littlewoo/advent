
import System.Environment
import Data.List.Split

main :: IO ()
main = do
            args <- getArgs
            text <- readFile (args !! 0)
            let n = (read :: String -> Int) (args !! 1) in
                let groups = map (map (read :: String -> Int)) (splitOn [""] (lines text)) in
                    print (sum (maxN n (map sum groups)))


maxN :: Int -> [Int] -> [Int]
maxN n xs = maxN' (take n $ repeat 0) xs

maxN' :: [Int] -> [Int] -> [Int]
maxN' acc []     = acc
maxN' acc (x:xs) = replaceBigger (maxN' acc xs) x 

replaceBigger :: [Int] -> Int -> [Int]
replaceBigger [] _     = []
replaceBigger (x:[]) n = [max x n]
replaceBigger (x:xs) n = if n > x then (n:(replaceBigger xs x)) else (x:(replaceBigger xs n))

