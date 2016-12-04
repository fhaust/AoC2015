import Data.Digest.Pure.MD5
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.List

check n key i = replicate n '0' `isPrefixOf` (show . md5 $ BS.pack (key ++ show i))

main = do

    let key = "yzbqklnj"

    let (Just result1) = elemIndex True [ check 5 key i | i <- [0..] ]
    let (Just result2) = elemIndex True [ check 6 key i | i <- [0..] ]

    putStrLn $ "result part one: " ++ show result1
    putStrLn $ "result part two: " ++ show result2
