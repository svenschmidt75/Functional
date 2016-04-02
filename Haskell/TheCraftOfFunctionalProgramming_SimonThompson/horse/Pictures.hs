module Pictures where

-- page 21

type Picture = [String]

horse :: Picture
horse = [".......##...",
         ".....##..#..",
         "...##.....#.",
         "..#.......#.",
         "..#...#...#.",
         "..#...###.#.",
         ".#....#..##.",
         "..#...#.....",
         "...#...#....",
         "....#..#....",
         ".....#.#....",
         "......##...."]

-- mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
printPicture :: Picture -> IO ()
printPicture = mapM_ putStrLn

flipH :: Picture -> Picture
flipH = reverse

flipV :: Picture -> Picture
flipV picture = [ reverse line | line <- picture ]
