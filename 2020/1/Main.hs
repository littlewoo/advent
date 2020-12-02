

import System.Environment
import Data.Maybe


main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            let nums = map (read :: (String -> Int)) (lines text) in
                print ((get2020 nums), (gen2020x3 nums))


get2020 :: [Int] -> (Int, Maybe (Int, Int))
get2020 xs = let u = unconverted xs in (convertResult u, u)

unconverted :: [Int] -> Maybe (Int, Int)
unconverted xs = findSummands 2020 (sort xs)

-- find a possible two values in xs that add up to n
findSummands :: Int -> [Int] -> Maybe (Int, Int)
findSummands n (x:[]) = Nothing
findSummands n (x:xs) = let other = findOtherSummand n x xs in
                                case other of
                                    Nothing -> findSummands n xs
                                    Just y -> Just (x, y)
      

findOtherSummand :: Int -> Int -> [Int] -> Maybe Int
findOtherSummand n x []         = Nothing
findOtherSummand n x (y:ys)     = if x + y == n then Just y else if x + y > n then Nothing else findOtherSummand n x ys

convertResult :: Maybe (Int, Int) -> Int
convertResult (Nothing)         = 0
convertResult (Just (x,y))      = x*y



gen2020x3 :: [Int] -> (Int, Maybe (Int, Int, Int))
gen2020x3 xs = let u = unconverted3 xs in (convert3Result u, u)

unconverted3 :: [Int] -> Maybe (Int, Int, Int) 
unconverted3 xs = find3Summands 2020 (sort xs)

find3Summands :: Int -> [Int] -> Maybe (Int, Int, Int)
find3Summands n []            = Nothing
find3Summands n (x:xs)        = let other2 = findLast2Summands n x xs in 
                                    case other2 of 
                                        Nothing         -> find3Summands n xs
                                        Just (y, z)     -> Just (x, y, z)

findLast2Summands :: Int -> Int -> [Int] -> Maybe (Int, Int)
findLast2Summands n x []            = Nothing
findLast2Summands n x (y:ys)        = if (x+y >= n) 
                                        then 
                                            Nothing 
                                        else 
                                            let other = findOtherSummand (n-x) y ys in
                                                case other of 
                                                    Nothing -> findLast2Summands n x ys
                                                    Just z  -> Just (y, z)

sort :: [Int] -> [Int]
sort []                 = []
sort (p:xs)             = (sort little) ++ [p] ++ (sort big) 
                            where 
                                little   = filter (< p) xs
                                big      = filter (>= p) xs
                    

convert3Result :: Maybe (Int, Int, Int) -> Int
convert3Result (Nothing)         = 0
convert3Result (Just (x, y, z))  = x*y*z
