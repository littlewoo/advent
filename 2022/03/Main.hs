
import System.Environment
import qualified Data.List as L
import qualified Data.Char as C

main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (puzzle1 text)
            print (puzzle2 text)

puzzle1 :: String -> String
puzzle1 text = show $ prio $ findAllCommonInEachPack $ lines text

puzzle2 :: String -> String
puzzle2 text = show $ prio $ findCommonInGroups $ lines text 

prio :: [Char] -> Int
prio = (sum . (map prio')) 

prio' :: Char -> Int
prio' c = if C.isUpper c then ((C.ord c) - (C.ord 'A') + 27) else ((C.ord c) - (C.ord 'a') + 1)

findAllCommonInEachPack :: (Ord a) => [[a]] -> [a] 
findAllCommonInEachPack = map (findCommonInPack . sorted . splitInHalf) 

findCommonInPack :: (Ord a) => ([a],[a]) -> a
findCommonInPack ([],_) = error "Reached end of pack"
findCommonInPack (_,[]) = error "Reached end of pack"
findCommonInPack ((x:xs),(y:ys)) = if x == y 
                                then x 
                                else if x < y 
                                    then findCommonInPack (xs, (y:ys))
                                    else findCommonInPack ((x:xs), ys) 

findCommonInGroups :: [[Char]] -> [Char]
findCommonInGroups text = map findCommonInGroup $ splitIntoThrees text

findCommonInGroup :: [[Char]] -> Char
findCommonInGroup sacks = let sortedSacks = map L.sort sacks in 
                              let heads = map head sortedSacks in
                                  if allEqual heads 
                                        then head heads 
                                        else let m = min' 'z' heads in
                                                findCommonInGroup $ beheadBy m sortedSacks

beheadBy :: (Eq a) => a -> [[a]] -> [[a]]
beheadBy _ []       = []
beheadBy a ((x:xs):xss) = if a == x then (xs:beheadBy a xss) else ((x:xs):beheadBy a xss)


smallestHead :: (Ord a) => a -> [[a]] -> a
smallestHead a []       = a
smallestHead a (x:xs)   = if a < (head x) then smallestHead a xs else smallestHead (head x) xs 
 
min' :: (Ord a) => a -> [a] -> a
min' a []        = a
min' a (x:xs)    = let a' = if a < x then a else x in min' a' xs

allEqual :: (Eq a) => [a] -> Bool
allEqual []             = True
allEqual (x:[])         = True
allEqual (x:x':xs)      = x == x' && allEqual (x':xs)

sorted :: (Ord a) => ([a],[a]) -> ([a],[a])
sorted (xs, ys) = (L.sort xs, L.sort ys)

splitInHalf :: [a] -> ([a],[a])
splitInHalf xs = let mid = ((length xs) `div` 2) in 
                    (take mid xs, drop mid xs)

splitIntoThrees :: [a] -> [[a]]
splitIntoThrees [] = []
splitIntoThrees xs = (take 3 xs:splitIntoThrees (drop 3 xs))
