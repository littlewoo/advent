
import System.Environment
import Data.Maybe


main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            let nums = map (read :: (String -> Int)) (lines text) in
                print (countIncreases (prev3WindowIsLesser nums))

countIncreases :: [Bool] -> Int
countIncreases bs = length (filter (\a -> a) bs)

prevIsLesser :: Maybe Int -> [Int] -> [Bool]
prevIsLesser _ [] = []
prevIsLesser Nothing (x:xs) = (False : prevIsLesser (Just x) xs)
prevIsLesser (Just p) (x:xs) = ((x > p) : prevIsLesser (Just x) xs)

prev3WindowIsLesser :: [Int] -> [Bool]
prev3WindowIsLesser [] = []
prev3WindowIsLesser (_:[]) = [] 
prev3WindowIsLesser (_:_:[]) = []
prev3WindowIsLesser (_:_:_:[]) = [] 
prev3WindowIsLesser (a:b:c:d:xs) = (d > a) : (prev3WindowIsLesser (b:c:d:xs))
