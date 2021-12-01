
import Text.Regex
import Data.List.Split
import qualified Data.Map as M
import qualified Data.List as L

data Tile = T TileId [Side] [[Bool]] deriving Eq
data TileId  = TileId Int deriving Eq
data Side = Side SideId [Bool] deriving Eq
data SideId = SideId TileId Orientation deriving Eq
data Orientation = N | E | S | W deriving (Show, Eq)
data Flip = F Bool deriving Show
-- Orientation is the side where the side that was N is - i.e. N is unchanged, E is rotated 90° clockwise, etc. 
-- Flipped is the axis it's flipped around - based on the unrotated data
data TilePlacement = P Tile Orientation Flip deriving Show
type PuzzleCompletion = [[TilePlacement]]

regexTileId = mkRegex "Tile ([0-9]*):"

instance Show Tile where
    show (T (TileId id) sides td) = "Tile " ++ show id ++ "\n" ++ "Sides:\n" ++ showSides sides ++ (concat (map showRow td))
instance Show Side where
    show (Side (SideId tid o) sd) = show o ++ ": " ++ showRow sd 

part2 :: String -> Int -> IO ()
part2 filename gridSize = do
                            rawinput <- readFile filename
                            let tiles = readInput rawinput
                            let completion = completePuzzle tiles gridSize
                            putStr $ showCompletion completion
                            

completePuzzle :: [Tile] -> Int -> PuzzleCompletion
completePuzzle ts gridSize = completePuzzle' ts gridSize 0 []

completePuzzle' :: [Tile] -> Int -> Int -> PuzzleCompletion -> PuzzleCompletion
completePuzzle' ts gridSize rowNumber soFar = if (rowNumber == gridSize) then soFar else do
                                                let rowStart@(P tile _ _) = if rowNumber == 0 
                                                        then findAndOrientACorner ts 
                                                        else placeFirstInRow ts (head $ last soFar)
                                                let (row, nextTs) = completeRow (L.delete tile ts) gridSize [rowStart]
                                                completePuzzle' nextTs gridSize (rowNumber + 1) (soFar ++ [row])
                                                

placeFirstInRow :: [Tile] -> TilePlacement -> TilePlacement
placeFirstInRow ts tileAbove = let (t,o,f) = findMatch (southFacingSide tileAbove) ts in P t o f
                               
completeRow :: [Tile] -> Int -> [TilePlacement] -> ([TilePlacement], [Tile])
completeRow ts gridSize rowSoFar = if (length rowSoFar) == gridSize 
                            then (rowSoFar, ts)
                            else do
                                let prev = last rowSoFar
                                let interface = rightFacingSide prev
                                let (t, o, f) = findMatch interface ts
                                let nextTs = L.delete t ts
                                completeRow nextTs gridSize (rowSoFar ++ [P t o f])

rightFacingSide :: TilePlacement -> (Side, Orientation, Flip)
rightFacingSide (P (T id sides td) o (F f)) = (sides !! (case f of
                                            False -> case o of 
                                                    N -> 1
                                                    E -> 0
                                                    S -> 3
                                                    W -> 2
                                            True -> case o of
                                                    N -> 3
                                                    E -> 0
                                                    S -> 1
                                                    W -> 2), E, F f) 
                                                 
southFacingSide :: TilePlacement -> (Side, Orientation, Flip)
southFacingSide (P (T id sides td) o (F f)) = (sides !! (case f of
                                            False -> case o of
                                                N -> 2
                                                E -> 1
                                                S -> 0
                                                W -> 3
                                            True -> case o of 
                                                N -> 2
                                                E -> 3
                                                S -> 0
                                                W -> 1), S, F f)                                               

placeFirstCorner :: [Tile] -> PuzzleCompletion
placeFirstCorner ts = [[findAndOrientACorner ts]]

findAndOrientACorner :: [Tile] -> TilePlacement
findAndOrientACorner ts = do 
                            let (t, ss) = findACorner ts 
                            let o = (case ss of
                                        [0,1] -> W
                                        [1,2] -> S
                                        [2,3] -> E
                                        [3,0] -> N)
                            P t o (F False)                            

showCompletion :: PuzzleCompletion -> String
showCompletion completion = concat $ map (\tr -> showRowOfTiles tr completion) [0..((length completion)-1)]

showRowOfTiles :: Int -> PuzzleCompletion -> String
showRowOfTiles gridRowNumber completion = let gridRow = completion !! gridRowNumber in 
                                            (concat $ (map (\r -> showRowAcrossTiles r gridRow) [0..((tileSize $ gridRow !! 0)-1)]))
                                                    
showRowAcrossTiles :: Int -> [TilePlacement] -> String
showRowAcrossTiles tileRowNumber gridRow = (concat $ (map (showRowWithinTile tileRowNumber) gridRow)) ++ "\n"

showRowWithinTile :: Int -> TilePlacement -> String
showRowWithinTile r tilePlacement = (map (\b -> if b then '#' else '.') ((getTilePlacementData tilePlacement) !! r))

tileSize :: TilePlacement -> Int
tileSize (P (T _ _ bs) _ _) = length bs  

getTilePlacementData :: TilePlacement -> [[Bool]]
getTilePlacementData (P (T _ _ td) o f) = ((reorient o) . (flipTile f)) td

reorient :: Orientation -> [[Bool]] -> [[Bool]]
reorient N b = b
reorient S b = (reverse . map reverse) b
reorient E b = (L.transpose . reverse) b 
reorient W b = (reverse . L.transpose) b 

flipTile :: Flip -> [[Bool]] -> [[Bool]]
flipTile (F False) b = b
flipTile (F True)  b = map reverse b

findMatch :: (Side, Orientation, Flip) -> [Tile] -> (Tile, Orientation, Flip)
findMatch _ []                                 = error "No match found"
findMatch (s, o, f) (tile@(T tid sides td):ts) = let candidates = filter (isPotentialMatch s) sides in
                                                   if (length candidates) == 0 
                                                     then findMatch (s, o, f) ts 
                                                     else findCorrectOrientationForKnownMatch (s, o, f) tile
                        

findCorrectOrientationForKnownMatch :: (Side, Orientation, Flip) -> Tile -> (Tile, Orientation, Flip)
findCorrectOrientationForKnownMatch (otherSide@(Side _ sideData), orientation, isFlipped) matchedTile@(T _ sides _) = do
    let matchedSide = head $ filter (\s -> isPotentialMatch (sides !! s) otherSide) [0..3]
    let side = sides !! matchedSide
    let F f = findWhetherFlippedForKnownMatch (sideData, isFlipped) side 
    let o = case orientation of
                E -> if f then case matchedSide of
                        0 -> W
                        1 -> N
                        2 -> E
                        3 -> S
                     else case matchedSide of
                        0 -> W
                        1 -> S
                        2 -> E
                        3 -> N
                S -> if f then case matchedSide of
                        0 -> N
                        1 -> E
                        2 -> S
                        3 -> W
                      else case matchedSide of
                        0 -> N        
                        1 -> W
                        2 -> S
                        3 -> E
    (matchedTile, o, F f)
        
findWhetherFlippedForKnownMatch :: ([Bool], Flip) -> Side -> Flip
findWhetherFlippedForKnownMatch (fixedSd, F flipped) (Side _ matchedSd) = F ((fixedSd == matchedSd) /= flipped)

countAllPotentialMatches :: [Tile] -> Int
countAllPotentialMatches ts = let sides = concat (map (\(T _ ss _) -> ss) ts) in sum (map (\s -> countPotentialMatches sides s) sides)

countPotentialMatches :: [Side] -> Side -> Int
countPotentialMatches allSides side = length $ filter (isPotentialMatch side) allSides

findCorners :: [Tile] -> [Tile]
findCorners ts = let sides = concat (map (\(T _ ss _) -> ss) ts) in filter (isCorner sides) ts

isCorner :: [Side] -> Tile -> Bool
isCorner allSides tile = (length $ nonMatchingSides allSides tile) == 2

findACorner :: [Tile] -> (Tile, [Int])
findACorner ts = do
                    let sides = concat (map (\(T _ ss _) -> ss) ts) 
                    let unmatchedSides = map (\t -> (t, nonMatchingSides sides t)) ts
                    (filter (\(_, ss) -> length ss == 2) unmatchedSides) !! 0

nonMatchingSides :: [Side] -> Tile -> [Int]
nonMatchingSides allSides (T _ tsides _) = filter (\s -> (countPotentialMatches allSides (tsides !! s)) == 0) [0..3]

isPotentialMatch :: Side -> Side -> Bool
isPotentialMatch (Side id1 sd1) (Side id2 sd2) = (id1 /= id2) && ((sd1 == sd2) || (sd1 == (reverse sd2)))

readInput :: String -> [Tile]
readInput ss = map readTile $ splitOn [""] $ lines ss

showSides :: [Side] -> String 
showSides [] = "\n"
showSides (b:bs) = show b ++ showSides bs

showRow :: [Bool] -> String
showRow [] = "\n"
showRow (b:bs) = ((if b then '#' else '.'):showRow bs) 

readTile :: [String] -> Tile
readTile (s:ss) = let id = readTileId s
                      td = readTileData ss in T id (parseSides id td) ((init . tail) $ map (init . tail) td)

parseImage :: [[Bool]] -> [[Bool]]
parseImage bs = map (init . tail) ((init . tail) bs)

parseSides :: TileId -> [[Bool]] -> [Side]
parseSides id td = [parseNorth id td, parseEast id td, parseSouth id td, parseWest id td]

parseNorth :: TileId -> [[Bool]] -> Side
parseNorth tid (bs:bss) = Side (SideId tid N) bs

parseEast :: TileId -> [[Bool]] -> Side
parseEast tid bss = Side (SideId tid E) (map last bss)

parseSouth :: TileId -> [[Bool]] -> Side 
parseSouth tid bss = Side (SideId tid S) (reverse (last bss))

parseWest :: TileId -> [[Bool]] -> Side
parseWest tid bss = Side (SideId tid W) (reverse $ map head bss)

readTileData :: [String] -> [[Bool]]
readTileData = map readTileRow

readTileRow :: String -> [Bool]
readTileRow = map (\c -> if c == '#' then True else False) 

readTileId :: String -> TileId
readTileId s = let (Just n) = matchRegex regexTileId s in TileId ((read :: String -> Int) (n !! 0))

-- shamelessly stolen from StackOverflow
squareRoot :: Integral t => t -> t
squareRoot n = babylon n
   where
   babylon a   | a > b  = babylon b
               | True   = a
      where b  = quot (a + quot n a) 2

