
import System.Environment
import Data.Char
import Data.List

main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (puzzle1 text)
            print (puzzle2 text)

puzzle1 :: String -> String
puzzle1 s = show $ sum $ map readValuesFromLine (lines s)

puzzle2 :: String -> String
puzzle2 s = let ss = lines s in show $ sum $ (map (*10) (firstDigits digitWords ss)) ++ (lastDigits digitWords ss) 

digitWords = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

lastDigits :: [String] -> [String] -> [Int]
lastDigits ss1 ss2 = firstDigits (map reverse ss1) (map reverse ss2)

firstDigits :: [String] -> [String] -> [Int]
firstDigits _           []     = []
firstDigits digitWords' (s:ss) = (firstDigit (subWordsForDigits digitWords' 8 s):firstDigits digitWords' ss) 

readValuesFromLine :: String -> Int
readValuesFromLine s = firstDigit s * 10 + firstDigit (reverse s)

firstDigit :: String -> Int
firstDigit []     = error "got to end of string without finding a digit"
firstDigit (x:xs) = if isDigit x then digitToInt x else firstDigit xs



subWordsForDigits :: [String] -> Int -> String -> String
subWordsForDigits digitWords' _    []     = []
subWordsForDigits digitWords' (-1) (s:ss) = (s:subWordsForDigits digitWords' 8 ss)
subWordsForDigits digitWords' n    s      = let word = digitWords' !! n in
                                             if word `isPrefixOf` s
								    	       then (show (n+1)) ++ subWordsForDigits digitWords' 8 (drop (length word) s) 
									           else subWordsForDigits digitWords' (n-1) s 


