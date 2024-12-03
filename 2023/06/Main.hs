
import System.Environment

main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (puzzle1 text)
            print (puzzle2 text)

puzzle1 :: String -> String
puzzle1 _ = "1: Not implemented yet."

puzzle2 :: String -> String
puzzle2 _ = "2: Not implemented yet." 


-- calculate all possible distances travelled in a race of t time
allPossibleDistancesForTime :: Int -> [Int]
allPossibleDistancesForTime t = map (distanceForTime t) [1..t]

-- calculate the distance travelled in t time if the button is held down for n milliseconds
distanceForTime :: Int -> Int -> Int
distanceForTime t n = if n > t 
						then error ("button is held (" ++ (show n) ++ ") longer than total race time (" ++ (show t) ++ ")")
						else (t-n) * n

