
import qualified Data.Map as Map
import System.Environment

main :: IO ()
main = do
            args <- getArgs
            let n = (read :: String -> Int) $ args !! 0;
                xs = (read :: String -> [Int]) $ args !! 1 in
                    print (playGame n xs)

type TurnNumber = Int
type PreviousTurns = Map.Map Int TurnNumber

previous :: [Int]
previous = [14,3,1,0,9,5]

initPrev :: [Int] -> PreviousTurns
initPrev xs = initPrev' (length xs) (reverse xs)

initPrev' :: Int -> [Int] -> PreviousTurns
initPrev' _ []     = Map.empty
initPrev' t (x:xs) = updateLatest x t $ initPrev' (t-1) xs

takeTurn :: (TurnNumber, Int, PreviousTurns) -> (TurnNumber, Int, PreviousTurns)
takeTurn (t, prevTurn, prev) = let l = calculateLatest t prevTurn prev in
                                 (t+1, l, updateLatest prevTurn (t-1) prev)

playGame :: Int -> [Int] -> Int
playGame turns xs = (\(_,n,_) -> n) $ applyNTimes (turns - (length xs)) takeTurn $ ((length xs)+1, last xs, initPrev xs)

--applyNTimes :: Int -> (a -> a) -> a -> a
--applyNTimes 0 _ x = x 
--applyNTimes n f x = applyNTimes (n-1) f $! f x

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f x = (iterate f x) !! n

updateLatest :: Int -> TurnNumber -> PreviousTurns -> PreviousTurns
updateLatest n t m = Map.insert n t m

calculateLatest :: TurnNumber -> Int -> PreviousTurns -> Int
calculateLatest t n m = case Map.lookup n m of 
                                       Nothing -> 0
                                       Just pr -> (t-1)-pr
