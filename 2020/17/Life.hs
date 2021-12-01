

import qualified Data.Set as S

type Living = S.Set Point
type Point = (Int, Int, Int)

births  = S.fromList [3]
survive = S.fromList [2,3]

relativeNeighbours = [((-1),(-1),(-1)), ((-1),(-1),( 0)), ((-1),(-1),( 1)), 
                      ((-1),( 0),(-1)), ((-1),( 0),( 0)), ((-1),( 0),( 1)), 
                      ((-1),( 1),(-1)), ((-1),( 1),( 0)), ((-1),( 1),( 1)),
 
                      (( 0),(-1),(-1)), (( 0),(-1),( 0)), (( 0),(-1),( 1)), 
                      (( 0),( 0),(-1)),                   (( 0),( 0),( 1)), 
                      (( 0),( 1),(-1)), (( 0),( 1),( 0)), (( 0),( 1),( 1)),   

                      (( 1),(-1),(-1)), (( 1),(-1),( 0)), (( 1),(-1),( 1)), 
                      (( 1),( 0),(-1)), (( 1),( 0),( 0)), (( 1),( 0),( 1)), 
                      (( 1),( 1),(-1)), (( 1),( 1),( 0)), (( 1),( 1),( 1))] 

part1 :: Living -> Int
part1 l = S.size $ applyNTimes generate 6 l

applyNTimes :: (a -> a) -> Int -> a -> a
applyNTimes _ 0 x = x
applyNTimes f n x = f (applyNTimes f (n-1) x)

generate :: Living -> Living
generate l = generatePoints (S.toList l) l

generatePoints :: [Point] -> Living -> Living
generatePoints ps l = S.fromList $ filter (\p -> isAliveNextGen p l) $ allPointsInBounds (S.toList l)

isAliveNextGen :: Point -> Living -> Bool
isAliveNextGen p l = let neighbours = countNeighbours p l 
                         isAlive = S.member p l in
                            (S.member neighbours survive && isAlive) ||
                            (S.member neighbours births && (not isAlive))


allPointsInBounds :: [Point] -> [Point]
allPointsInBounds livingPoints = let ((lx,ly,lz),(ux,uy,uz)) = bounds livingPoints in
                                    [(x,y,z) | x <- [(lx-1)..(ux+1)], y <- [(ly-1)..(uy+1)], z <- [(lz-1)..(uz+1)] ]
                                    

bounds :: [Point] -> (Point, Point)
bounds ps = bounds' ps ((ps !! 0), (ps !! 0))

bounds' :: [Point] -> (Point, Point) -> (Point, Point)
bounds' [] b = b
bounds' ((x,y,z):ps) ((lx, ly, lz),(ux, uy, uz)) = bounds' ps ((min x lx, min y ly, min z lz), (max x ux, max y uy, max z uz))

countNeighbours :: Point -> Living -> Int
countNeighbours p f = S.size $ S.intersection (S.fromList (refNeighbours p)) f

refNeighbours :: Point -> [Point]
refNeighbours p = map (addPoints p) relativeNeighbours

addPoints :: Point -> Point -> Point
addPoints (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

showSpace :: Living -> String
showSpace l = showCube l (bounds $ S.toList l)
          
showCube :: Living -> (Point, Point) -> String
showCube l ((lx,ly,lz),(ux,uy,uz)) = concat $ map (\z -> showSlice l ((lx,ly),(ux,uy)) z) [lz..uz]           

showSlice :: Living -> ((Int, Int), (Int,Int)) -> Int -> String
showSlice l ((lx,ly),(ux,uy)) z = (concat $ map (\y -> showRow l (lx,ux) y z) [ly..uy]) ++ "\n"

showRow :: Living -> (Int, Int) -> Int -> Int -> String
showRow l (lx, ux) y z = let ps = map (\x -> (x,y,z)) [lx..ux] in (map (showPoint l) ps ++ "\n")

showPoint :: Living -> Point -> Char
showPoint l p = if S.member p l then '#' else '.'

readSlice :: [String] -> Int -> Int -> Living
readSlice []     _ _ = S.empty
readSlice (s:ss) y z = S.union (readSlice ss (y+1) z) (readRow s 0 y z)

readRow :: String -> Int -> Int -> Int -> Living
readRow []     _ _ _ = S.empty
readRow (c:cs) x y z = let row = readRow cs (x+1) y z in
                            if c == '#'
                                 then S.insert (x,y,z) row
                                 else row
            
                        

