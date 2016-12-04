
import qualified Data.Map.Strict as Map

step (x,y) '^' = (x+0,y+1)
step (x,y) '>' = (x+1,y+0)
step (x,y) 'v' = (x+0,y-1)
step (x,y) '<' = (x-1,y+0)

roboSanta (s,r) (a:b:rst) = let s' = step s a; r' = step r b in (s',r') : roboSanta (s',r') rst
roboSanta _            [] = []

countVisits ps = Map.fromListWith (+) $ zip ps (repeat 1)

main = do

    instructions <- readFile "input-3.txt"

    let positions1 = scanl step (0,0) instructions
        positions2 = uncurry (++) . unzip $ roboSanta ((0,0),(0,0)) instructions

    let result1 = Map.size $ countVisits positions1
        result2 = Map.size $ countVisits positions2


    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
