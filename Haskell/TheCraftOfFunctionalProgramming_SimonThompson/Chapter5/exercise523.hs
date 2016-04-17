module Exercise523 where


duplicate :: String -> Integer -> String
duplicate s n
    | n == 0 = []
    | n == 1 = s
    | otherwise = s ++ duplicate s (n - 1)

main :: IO ()
main = do
    print (duplicate "A1" 10)
