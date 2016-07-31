module Main where


fldr :: (a -> b -> b) -> b -> [a] -> b
fldr f a [] = a
fldr f a (x:xs) = f x (fldr f a xs)

main :: IO ()
main = do
		putStrLn $ show $ fldr (+) (0 :: Int) [1, 2, 3]
