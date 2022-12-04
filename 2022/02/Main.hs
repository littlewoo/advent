
import System.Environment
import qualified Data.Map as Map

main :: IO ()
main = do
        args <- getArgs
        text <- readFile (args !! 0)
        let lns = lines text in print (sum $ scoreMatch lns)

data Hand = Rock | Paper | Scissors | Err deriving (Show, Eq, Ord)
data Target = Win | Lose | Draw deriving (Show, Eq, Ord)

moveLookup :: Map.Map (Hand, Target) Hand
moveLookup = Map.fromList  [(( Rock,     Win  ), Paper    ),
                        (( Rock,     Draw ), Rock     ),
                        (( Rock,     Lose ), Scissors ),
                        (( Paper,    Win  ), Scissors ),
                        (( Paper,    Draw ), Paper    ),
                        (( Paper,    Lose ), Rock     ),
                        (( Scissors, Win  ), Rock     ),
                        (( Scissors, Draw ), Scissors ),
                        (( Scissors, Lose ), Paper    )] 

scoreMatch :: [String] -> [Int]
scoreMatch m = score $ map (\l -> (theirHand l, myHand l)) m

score :: [(Hand, Hand)] -> [Int]
score = map (\(t,m) -> (scoreMyHand m) + (gameScore (t,m)))

scoreMyHand :: Hand -> Int
scoreMyHand h = case h of 
                    Rock        -> 1
                    Paper       -> 2
                    Scissors    -> 3

gameScore :: (Hand, Hand) -> Int
gameScore (t, m) = if (t == m) then 3 else case t of 
                                                Rock     -> if m == Paper       then 6 else 0
                                                Paper    -> if m == Scissors    then 6 else 0
                                                Scissors -> if m == Rock        then 6 else 0  

playMatch :: [String] -> [Int]
playMatch ss = map playTarget (strategy ss)

strategy :: [String] -> [(Hand, Target)]
strategy = map (\l -> (theirHand l, myTarget l))

playTarget :: (Hand, Target) -> Int
playTarget (t, m) = let myMove = Map.findWithDefault Err (t, m) moveLookup in 
                        gameScore (t, myMove) + scoreMyHand myMove 
                        

theirHand :: String -> Hand
theirHand (h:_:_:[]) = case h of
                            'A' -> Rock
                            'B' -> Paper
                            'C' -> Scissors
                            _   -> error "unknown sign"

myTarget :: String -> Target
myTarget (_:_:t:[]) = case t of 
                                'X' -> Lose
                                'Y' -> Draw
                                'Z' -> Win
                                _   -> error "unknown sign"

myHand :: String -> Hand
myHand (_:_:h:[]) = case h of 
                            'X' -> Rock
                            'Y' -> Paper
                            'Z' -> Scissors
                            _   -> error "unknown sign"   
