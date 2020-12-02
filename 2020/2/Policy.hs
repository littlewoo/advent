

import System.Environment

main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (countPolicyMatches (lines text))


type Policy    = ((Int, Int), Char)

countPolicyMatches :: [String] -> ([(Policy, String, Int, Bool)], Int)
countPolicyMatches xs = let input = checkPolicy xs in (input, length (filter (\(_,_,_,m) -> m) input))

checkPolicy :: [String] -> [(Policy, String, Int, Bool)]
checkPolicy xs = (map (\((mm, c), pas) -> ((mm, c), pas, matchingChars c pas, matchesNewPolicy (mm, c) pas)) (map parseLine xs))

matchesPolicy :: Policy -> String -> Bool
matchesPolicy ((min, max), c) s       = let len = (matchingChars c s) in len <= max && len >= min

matchesNewPolicy :: Policy -> String -> Bool
matchesNewPolicy ((pos1, pos2), c) pass = ((pass !! (pos1 - 1)) == c) && ((pass !! (pos2 - 1)) /= c) || ((pass !! (pos1 - 1)) /= c) && ((pass !! (pos2 - 1)) == c)

matchingChars :: Char -> String -> Int
matchingChars c s       = length (filter (== c) s)

parseLine :: String -> (Policy, String)
parseLine s     = (parsePolicy s, parsePassword s)

parsePolicy :: String -> Policy
parsePolicy s   = (((parseMin s), (parseMax s)), (parseChar s)) 

parseMin :: String -> Int
parseMin s      = (read :: String -> Int) (takeWhile (/= '-') s)

parseMax :: String -> Int
parseMax s      = (read :: String -> Int) (takeWhile (/= ' ') (tail (dropWhile (/= '-') s)))

parseChar :: String -> Char
parseChar s     = head (tail (dropWhile (/= ' ') s))

parsePassword :: String -> String
parsePassword s = drop 2 (dropWhile (/= ':') s)
