
import Data.List
import Data.List.Split

parseInput :: String -> [[Int]]
parseInput = map (map read . splitOn "x") . lines

wrapping [l,w,h] = 2*l*w + 2*w*h + 2*h*l + min (l*w) (min (w*h) (h*l))

ribbon [l,w,h] = 2*a + 2*b + l*w*h
    where [a,b,_] = sort [l,w,h]

main = do

    sizes <- parseInput <$> readFile "input-2.txt"

    let total1 = sum $ map wrapping sizes
        total2 = sum $ map ribbon sizes



    putStrLn $ "result part one: " ++ show total1
    putStrLn $ "result part two: " ++ show total2
