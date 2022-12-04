
import System.Environment
import Data.List
import Data.Char


filterToOne :: ([[Int]] -> [Int]) -> Int -> [[Int]] -> [Int]
filterToOne f col values = if (length values == 1) 
                                    then (values !! 0) 
                                    else filterToOne f (col + 1) (filterOnNthColumn col (f (transpose values)) values)

filterOnNthColumn :: Int -> [Int] -> [[Int]] -> [[Int]]
filterOnNthColumn _ _ []                = []
filterOnNthColumn n pattern (x:xs)      = let next = filterOnNthColumn n pattern xs in 
                                                if (x !! n) == (pattern !! n) then (x:next) else next

mostCommon :: [[Int]] -> [Int]
mostCommon columns = map (\x -> if (2*x) >= (length (columns !! 0)) then 1 else 0) (map sum columns) 

leastCommon :: [[Int]] -> [Int]
leastCommon columns = map (\x -> if (2*x) <= (length (columns !! 0)) then 1 else 0) (map sum columns) 


columnNums :: [String] -> [[Int]]
columnNums rows = map (map readChar) (transpose rows)

readChar c = if c == '0' then 0 else 1

backToChar :: Int -> Char
backToChar n = if (n == 0) then '0' else '1'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
