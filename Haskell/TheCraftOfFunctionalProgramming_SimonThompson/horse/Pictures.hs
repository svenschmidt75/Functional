module Pictures where

type Picture = [String]

horse :: Picture
horse = [".......##...", ".....##..#.."]

-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
printPicture :: Picture -> IO ()
printPicture = mapM_ putStrLn
