

import System.Environment
import qualified Data.Map as Map


main :: IO ()
main = do 
            args <- getArgs
            text <- readFile (args !! 0)
            print (let l = lines text in (part1 l, part2 l))            

type Action = Int -> ShipState -> ShipState 
type ShipState = ((Int, Int),(Int,Int))


actions = Map.fromList [('N', north), 
                        ('S', south),
                        ('E', east),
                        ('W', west),
                        ('L', left),
                        ('R', right),
                        ('F', forward)]

part1 :: [String] -> Int
part1 ss = manhattanDistance $ fst $ takeActions (parseActions ss) ((0,0),(0,0))

part2 :: [String] -> Int
part2 ss = manhattanDistance $ fst $ takeActions (parseActions ss) ((0,0), (10,1))

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x,y) = (abs x) + (abs y)

parseActions :: [String] -> ([ShipState -> ShipState])
parseActions ss = map parseAction ss

parseAction :: String -> (ShipState -> ShipState)
parseAction s = ((Map.!) actions (head s)) ((read :: String -> Int) (tail s))

takeActions :: [(ShipState -> ShipState)] -> ShipState -> ShipState
takeActions []     s = s
takeActions (f:fs) s = takeActions fs $ f s  

north :: Action
north s (loc,(wx,wy))     = (loc,(wx,wy+s))

south :: Action
south s (loc,(wx,wy))     = (loc,(wx,wy-s))  

east :: Action
east s (loc,(wx,wy))      = (loc,(wx+s,wy)) 

west :: Action
west s (loc,(wx,wy))      = (loc,(wx-s,wy))  

left :: Action
left s (loc,(wx,wy))      = (loc, applyNTimes (s `div` 90) (\(x,y) -> (-y,x)) (wx, wy)) 

right :: Action
right s (loc,(wx,wy))     = (loc, applyNTimes (s `div` 90) (\(x,y) -> (y,-x)) (wx, wy))

forward :: Action
forward s ((x,y),(wx,wy)) = (applyNTimes s (\(sx,sy) -> (sx+wx,sy+wy)) (x,y), (wx,wy)) 

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes 1 f x = f x
applyNTimes n f x = applyNTimes (n-1) f (f x)
