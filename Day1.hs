
import Data.List

main = do
    instructions <- readFile "input-1.txt"
    let positions = scanl (\e i -> if i == '(' then e+1 else e-1) 0 instructions

    let (Just firstBasement) = findIndex (<0) positions


    putStrLn $ "result part one: " ++ show (last positions)
    putStrLn $ "result part two: " ++ show firstBasement
