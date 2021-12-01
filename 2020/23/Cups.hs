
import qualified Data.List as L

testinput = [3,8,9,1,2,5,4,6,7] :: Cups
input     = [3,6,2,9,8,1,7,5,4] :: Cups

type Cups = [Int]

main :: IO ()
main = do
            let cups = concat [testinput, [10..1000000]]
            let endstate = playGame 10000000 0 cups
            print (take 100 endstate)


playGame :: Int -> Int -> Cups -> Cups
playGame turns curI cups = snd $ last $ take (turns+1) $ iterate playTurn (curI, cups)
                            
playTurn :: (Int, Cups) -> (Int, Cups)                        
playTurn (curI, cups) = do
                          let (remaining, removed)      = removeSublistFrom cups (curI+1) 3
                          let updated                   = insertCups (cups !! curI) remaining removed
                          let (Just currentInUpdated)   = L.elemIndex (cups !! curI) updated
                          let newCurI                   = (currentInUpdated + 1) `mod` (length cups)
                          (newCurI, updated)

insertCups :: Int -> Cups -> [Int] -> Cups
insertCups current cups toInsert = do 
                                     let sorted                 = L.sort cups
                                     let (Just currentIndex)    = L.elemIndex current sorted
                                     let destinationValueIndex  = (if currentIndex == 0 then length sorted else currentIndex) - 1
                                     let destinationValue       = sorted !! destinationValueIndex
                                     let (Just destIndex)       = L.elemIndex destinationValue cups
                                     insertSublistAt cups toInsert (destIndex + 1)

insertSublistAt :: [a] -> [a] -> Int -> [a]
insertSublistAt xs ys i = let (before, after) = L.splitAt i xs in before ++ ys ++ after
                    
removeSublistFrom :: [a] -> Int -> Int -> ([a],[a])
removeSublistFrom xs i n = if (i+n) < (length xs) 
                             then do
                                let (before,back)       = L.splitAt i xs
                                let (removed,after)     = L.splitAt n back
                                (before++after, removed)                                
                             else do
                                let fromEnd             = (length xs) - i
                                let fromStart           = n - fromEnd
                                let fromMiddle          = (length xs) - n
                                let (pre,rem)           = L.splitAt fromStart xs
                                let (middle,post)       = L.splitAt fromMiddle rem
                                (middle, post++pre)
