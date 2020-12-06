
import System.Environment


main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            let rows = (lines text) in
                print (findGap (sort (seatIndices rows)))

-- takes a sorted list and finds the first gap
findGap :: [Int] -> Int
findGap (x1:x2:xs) = if (x2 - x1) /= 1 then x2 else findGap (x2:xs)

sort :: [Int] -> [Int]
sort []                 = []
sort (p:xs)             = (sort little) ++ [p] ++ (sort big) 
                            where 
                                little   = filter (< p) xs
                                big      = filter (>= p) xs

seatIndices :: [String] -> [Int]
seatIndices xs        = map (\x -> snd (readSeat x)) xs

readSeat :: String -> ((Int, Int), Int)
readSeat s            = ((x, y), x*8+y) where 
                            x = readRow (0, 127) (take 7 s)
                            y = readColumn (0, 7) (drop 7 s)

readRow :: (Int, Int) -> String -> Int
readRow (l, u) []        = l
readRow (l, u) (x:xs)    = if x == 'F' then (readRow (lowerPart (l,u)) xs) else (readRow (upperPart (l,u)) xs)

readColumn :: (Int, Int) -> String -> Int
readColumn (l, u) []     = l
readColumn (l, u) (x:xs) = if x == 'L' then (readColumn (lowerPart (l,u)) xs) else (readColumn (upperPart (l,u)) xs)

upperPart :: (Int, Int) -> (Int, Int)
upperPart (l, u) = (l + ((u-l) `div` 2) + 1, u)

lowerPart :: (Int, Int) -> (Int, Int)
lowerPart (l, u) = (l, l + ((u-l) `div` 2))
