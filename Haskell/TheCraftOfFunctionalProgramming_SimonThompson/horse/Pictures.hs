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

beside :: Picture -> Picture -> Picture
beside leftPicture rightPicture = [ l ++ r | (l, r) <- zip leftPicture rightPicture]

above :: Picture -> Picture -> Picture
above topPicture bottomPicture = topPicture ++ bottomPicture

invertPixel :: Char -> Char
invertPixel c = if c == '.' then '#' else '.'

invertColor :: Picture -> Picture
invertColor picture = [ map invertPixel line | line <- picture ]

main =
    printPicture (beside (flipV horse) horse)
