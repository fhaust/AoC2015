


import Data.List

nice s = threeVovels s && twiceLetter s && not (tabu s)
threeVovels = (>= 3) . length . filter (`elem` "aeiou")
twiceLetter s = not . null . filter (\(a,b) -> a == b) $ zip s (tail s)
tabu s = or [ t `isInfixOf` s | t <- ["ab","cd","pq","xy"] ]



nice2 s = rule1 s && rule2 s

rule1 (a:b:[])  = False
rule1 (a:b:rst) = [a,b] `isInfixOf` rst || rule1 (b:rst)

rule2 (a:b:[])    = False
rule2 (a:b:c:rst) = a == c || rule2 (b:c:rst)

main = do
    words <- lines <$> readFile "input-4.txt"

    let result1 = length . filter nice $ words
    let result2 = length . filter nice2 $ words


    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
