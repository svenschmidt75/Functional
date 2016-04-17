module Exercise522 where

onSeparateLines :: [String] -> String
onSeparateLines xs = foldr (\x y -> x ++ ['\n'] ++ y) [] xs

main :: IO ()
main = do
    putStrLn (onSeparateLines ["A1", "B2", "C3"])
