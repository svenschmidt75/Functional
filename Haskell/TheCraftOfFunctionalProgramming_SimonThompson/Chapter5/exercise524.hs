module Exercise524 where


pushRight :: Int -> String -> String
pushRight linelength s = let len = length s in
                            take (linelength - len) (repeat ' ') ++ s

main :: IO ()
main = do
    print (pushRight 12 "crocodile")
