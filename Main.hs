
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


