
import qualified Data.IntMap as Map
import System.Environment

main :: IO ()
main = do
            args <- getArgs
            let n = (read :: String -> Int) $ args !! 0;
                xs = (read :: String -> [Int]) $ args !! 1 in
                    print (playGame n xs)

type TurnNumber = Int
type PreviousTurns = Map.IntMap Int

previous :: PreviousTurns 
previous = Map.fromList [(14,1),(3,2),(1,3),(0,4),(9,5),(5,6)]

initPrev :: [Int] -> PreviousTurns
initPrev xs = initPrev' (length xs) (reverse xs)

initPrev' :: Int -> [Int] -> PreviousTurns
initPrev' _ []     = Map.empty
initPrev' t (x:xs) = updateLatest x t $ initPrev' (t-1) xs

takeTurn :: (TurnNumber, Int, PreviousTurns) -> (TurnNumber, Int, PreviousTurns)
takeTurn (t, prevTurn, prev) = let l = calculateLatest t prevTurn prev in
                                 (t+1, l, updateLatest prevTurn (t-1) prev)

playTurn :: Int -> (TurnNumber, Int, PreviousTurns) -> Int
playTurn 0 (_, prevTurn, _) = prevTurn
playTurn n (t, prevTurn, prev) = let l = calculateLatest t prevTurn prev in
                                   playTurn (n-1) (t+1, l, updateLatest prevTurn (t-1) prev)

playGame :: Int -> [Int] -> Int
--playGame turns xs = (\(_,n,_) -> n) $ applyNTimes (turns - 6) takeTurn $ (7, 5, previous)
playGame turns _ = playTurn (turns-6) (7,5,previous)

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
