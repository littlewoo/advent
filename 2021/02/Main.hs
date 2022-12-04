


import System.Environment
import Data.List


main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            let raw = (lines text) in
                    print (doActions (0,0,0) (map parseCommand raw))

data Command = Forward | Up | Down deriving (Show)        

doActions :: (Int, Int, Int) -> [(Command, Int)] -> (Int, Int, Int)
doActions pos [] = pos
doActions pos (c:cs) = doActions (applyCommand pos c) cs

applyCommand :: (Int, Int, Int) -> (Command, Int) -> (Int, Int, Int)
applyCommand (x, y, a) (Forward, n) = (x+n, y+(a*n),a)
applyCommand (x, y, a) (Up, n)      = (x, y, a-n)
applyCommand (x, y, a) (Down, n)    = (x, y, a+n)

parseCommand :: String -> (Command, Int)
parseCommand cmd = (parseAction (takeWhile (/= ' ') cmd), (read :: (String -> Int)) (tail (dropWhile (/= ' ') cmd)))

parseAction :: String -> Command
parseAction "forward" = Forward
parseAction "up"      = Up
parseAction "down"    = Down
