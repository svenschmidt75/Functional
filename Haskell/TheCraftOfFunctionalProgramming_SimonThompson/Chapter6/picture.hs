module Picture where

import Test.QuickCheck hiding (Result)


type Picture = [[Char]]

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


flipH :: Picture -> Picture
flipH = reverse

flipV :: Picture -> Picture
--flipV = map reverse
flipV pic = [reverse line | line <- pic]

above :: Picture -> Picture -> Picture
above = (++)

beside :: Picture -> Picture -> Picture
--beside left right = [leftLine ++ rightLine | (leftLine, rightLine) <- zip left right]
beside = zipWith (\leftLine rightLine -> leftLine ++ rightLine)


invertChar :: Char -> Char
invertChar ch = if ch == '.' then '#' else '.'

invertLine :: [Char] -> [Char]
invertLine line = [invertChar ch | ch <- line]

invertColor :: Picture -> Picture
--invertColor pic = [invertLine line | line <- pic]
invertColor pic = [[invertChar ch | ch <- line] | line <- pic]


superimposeChar :: Char -> Char -> Char
superimposeChar ch1 ch2 = if ch1 == ch2 && ch1 == '.' then '.' else '#'

superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine line1 line2 = [superimposeChar ch1 ch2 | (ch1, ch2) <- zip line1 line2]

superimpose :: Picture -> Picture -> Picture
superimpose pic1 pic2 = [superimposeLine line1 line2 | (line1, line2) <- zip pic1 pic2]

printPicture :: Picture -> IO ()
printPicture pic = putStrLn pics
            where
                pics = foldr (\l r -> l ++ "\n" ++ r) "" pic

getColumn :: Int -> Picture -> [Char]
getColumn idx pic = foldr (\a b -> b ++ [(a !! idx)]) "" pic

rotate90 :: Picture -> Picture
rotate90 pic = map (\idx -> getColumn idx pic) [0..(length pic - 1)]



prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV pic1 pic2 = flipV (pic1 `above` pic2) == (flipV pic1) `above` (flipV pic2)

prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH pic1 pic2 = flipH (pic1 `above` pic2) == (flipH pic2) `above` (flipH pic1)



main :: IO ()
main = do
    quickCheck prop_AboveFlipV
    quickCheck prop_AboveFlipH
    putStrLn "Test"
