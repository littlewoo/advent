
import Data.List.Split

type Deck = [Int]
data Winner = P1 | P2 deriving (Eq, Show)

part1 :: String -> IO ()
part1 s = do
            let decks = parseInput s
            print (playGame decks)

part2 :: String -> IO ()
part2 s = print (playRecursiveGame $ parseInput s)
           
playRecursiveGame :: (Deck, Deck) -> Int
playRecursiveGame (p1,p2) = fst $ scoreDeck $ snd $ playRecursiveRound [] (p1,p2)

playRecursiveRound :: [(Deck, Deck)] -> (Deck, Deck) -> (Winner, Deck)
playRecursiveRound prev (p1, p2) = if (p1,p2) `elem` prev then (P1, p1) else do
                                        let (p1a, p2a) = (if ((head p1) <= (length $ tail p1)) && 
                                                             ((head p2) <= (length $ tail p2)) 
                                                                then playRecursiveGameAsRound (p1, p2)
                                                                else playTurn (p1, p2))
                                        if p1a == [] then (P2, p2a) else 
                                            if p2a == [] then (P1, p1a) else
                                                playRecursiveRound ((p1,p2):prev) (p1a,p2a)

playRecursiveGameAsRound :: (Deck, Deck) -> (Deck, Deck)
playRecursiveGameAsRound ((p1:p1d),(p2:p2d)) = case fst $ playRecursiveRound [] (take p1 p1d, take p2 p2d) of
                                                  P1 -> (p1d ++ [p1, p2], p2d)
                                                  P2 -> (p1d, p2d ++ [p2, p1])
 
playGame :: (Deck, Deck) -> Int
playGame decks = let (p1,p2) = playTurn decks in
                    if p1 == [] then fst $ scoreDeck p2 else
                        if p2 == [] then fst $ scoreDeck p1 else
                            playGame (p1,p2)

scoreDeck :: Deck -> (Int,Int)
scoreDeck []     = (0,1)
scoreDeck (x:xs) = let (score,multiplier) = scoreDeck xs in (score + x*multiplier,multiplier+1)

playTurn :: (Deck, Deck) -> (Deck, Deck)
playTurn (p1,p2) = if (head p1 > head p2) 
                        then (tail p1 ++ [head p1, head p2], tail p2)
                        else (tail p1, tail p2 ++ [head p2, head p1])

parseInput :: String -> (Deck, Deck)
parseInput s = let sss = splitOn [""] $ lines s in (parsePlayerDeck (sss !! 0), parsePlayerDeck (sss !! 1))
                  

parsePlayerDeck :: [String] -> Deck
parsePlayerDeck ss = map (read :: String -> Int) (tail ss)
