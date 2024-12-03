
import System.Environment

data Direction = Increasing | Decreasing | Unknown deriving Show

main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (puzzle1 text)
            print (puzzle2 text)

puzzle1 :: String -> Int
puzzle1 input = length $ filter (isSafe Unknown) $ parseInput input

parseInput :: String -> [[Int]]
parseInput input = map (map (read :: String -> Int)) $ map words $ lines input

isSafe :: Direction -> [Int] -> Bool
isSafe _ []                 = True
isSafe _ (x:[])             = True
isSafe Unknown (x1:x2:xs)   = isSafe' Unknown x1 x2 && 
                               ((x1 < x2 && isSafe Increasing (x2:xs)) || 
                                (x1 > x2 && isSafe Decreasing (x2:xs)))
isSafe dir (x1:x2:xs) = isSafe' dir x1 x2 && isSafe dir (x2:xs)

puzzle2 :: String -> Int
puzzle2 text = length $ filter isSafeDamped $ parseInput text 

isSafeDamped :: [Int] -> Bool
isSafeDamped xs = isSafe Unknown xs || isSafeDamped' xs 0

isSafeDamped' :: [Int] -> Int -> Bool
isSafeDamped' xs n = (not (n >= length xs)) && (isSafe Unknown (dropIndex xs n) || isSafeDamped' xs (n+1)) 

dropIndex :: [a] -> Int -> [a]
dropIndex xs n = (take n xs) ++ (drop (n+1) xs)

{-
isSafeDamped :: [Int] -> Bool
isSafeDamped xs = (dampIsSafe (determineDirection xs) xs) <= 1 || (isSafe Unknown $ tail xs) || (isSafe Unknown $ (head xs:tail (tail xs)))

determineDirection :: [Int] -> Direction
determineDirection (x1:x2:_) = if x1 < x2 then Increasing else Decreasing

dampIsSafe :: Direction -> [Int] -> Int
dampIsSafe _   []            = 0
dampIsSafe _   (x:[])        = 0
dampIsSafe dir (x1:x2:[])    = isSafeN dir x1 x2
dampIsSafe dir (x1:x2:x3:xs) = if (isSafe' dir x1 x2) 
                                  then dampIsSafe dir (x2:x3:xs) 
                                  else 1 + dampIsSafe dir (x1:x3:xs)   

isSafeN :: Direction -> Int -> Int -> Int
isSafeN dir x y = if (isSafe' dir x y) then 0 else 1
-}

isSafe' :: Direction -> Int -> Int -> Bool
isSafe' Unknown x y     = x /= y && abs (x-y) <= 3
isSafe' Increasing x y  = x  < y && abs (x-y) <= 3 
isSafe' Decreasing x y  = x  > y && abs (x-y) <= 3 
