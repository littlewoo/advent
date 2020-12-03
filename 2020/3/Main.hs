

import System.Environment


main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            let rows = (lines text) in
                print (countAllRows rows)

countAllRows :: [[Char]] -> Int
countAllRows xs = product (map (\xy -> countRows 0 0 xy xs) [(1,1), (3,1), (5,1), (7,1), (1,2)] )

countRows :: Int -> Int -> (Int, Int) -> [[Char]] -> Int
countRows n _ _ []                  = n
countRows n col (slopeX, slopeY) ts = countRows (n+tree) ((col + slopeX) `mod` length (ts !! 0)) (slopeX, slopeY) (skipRows slopeY ts)
                                                where tree = countTree slopeY (col+slopeX) ts

skipRows :: Int -> [[Char]] -> [[Char]]
skipRows n xs = if n < (length xs) then (drop n xs) else []

countTree :: Int -> Int -> [[Char]] -> Int
countTree _ _ []         = 0
countTree slopeY col ts  = let rows = skipRows slopeY ts in
                                if rows == [] 
                                    then 0
                                    else let row = rows !! 0 in 
                                        if (row !! (col `mod` (length row))) == '#' 
                                            then 1 
                                            else 0   
