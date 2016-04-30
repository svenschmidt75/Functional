module Picture where

import Test.QuickCheck hiding (Result, scale)


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

-- We assume that Picture's are rectangular
pictureMaxWidth :: Picture -> Int
pictureMaxWidth pic = maximum $ map length pic

padLineToWidth :: [Char] -> Int -> [Char]
padLineToWidth line width
    | length line >= width = line
    | otherwise            = line ++ (replicate diff '.')
                            where
                                diff = width - length line

getMaxPictureWidth :: Picture -> Int
getMaxPictureWidth pic = pictureMaxWidth pic

padPictureToWidth :: Picture -> Picture
padPictureToWidth pic = if diff > 0 then
                            [padLineToWidth line nRows | line <- pic]
                        else
                            pic
                    where
                        nRows      = length pic
                        nColumns   = getMaxPictureWidth pic
                        diff       = nRows - nColumns

padPictureToHeight :: Picture -> Picture
padPictureToHeight pic = if diff > 0 then pic ++ (replicate diff paddedLine) else pic
                    where
                        nRows      = length pic
                        nColumns   = getMaxPictureWidth pic
                        diff       = nColumns - nRows
                        paddedLine = replicate nColumns '.'

makePictureRectangular :: Picture -> Picture
makePictureRectangular = padPictureToWidth . padPictureToHeight

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
rotate90 pic = 
            let nColumns = length pic - 1 in
            map (\idx -> getColumn idx pic) [0..nColumns]

rotate90Anti :: Picture -> Picture
rotate90Anti = flipV . flipH . rotate90


multiplyChar :: Char -> Int -> [Char]
multiplyChar ch scale = replicate scale ch

multiplyCharInLine :: [Char] -> Int -> [Char]
multiplyCharInLine line scale
    | scale <= 0 = []
    | otherwise  = concat [multiplyChar ch scale | ch <- line]

scale :: Picture -> Int -> Picture
scale pic idx = concat $ map func pic
            where
                func line = let l = multiplyCharInLine line idx in
                            replicate idx l



prop_AboveFlipV :: Picture -> Picture -> Bool
prop_AboveFlipV pic1 pic2 = flipV (pic1 `above` pic2) == (flipV pic1) `above` (flipV pic2)

prop_AboveFlipH :: Picture -> Picture -> Bool
prop_AboveFlipH pic1 pic2 = flipH (pic1 `above` pic2) == (flipH pic2) `above` (flipH pic1)

prop_FourCopies :: Picture -> Bool
prop_FourCopies pic = (pic `above` pic) `beside` (pic `above` pic) == (pic `beside` pic) `above` (pic `beside` pic)

prop_IsRectangular :: Picture -> Property
prop_IsRectangular pic =
     (null pic == False)
    ==>
     length (makePictureRectangular pic) == getMaxPictureWidth (makePictureRectangular pic)

{-
propAboveBeside3Correct :: Picture -> Picture -> Picture
propAboveBeside3Correct w e =
    (rectangular w && rectangular e && height w == height e)
    ==>
    (w `beside` e) `above` (w `beside` e) == (w `above` w) `above` (e `beside` e)
-}

main :: IO ()
main = do
    quickCheck prop_AboveFlipV
    quickCheck prop_AboveFlipH
    quickCheck prop_FourCopies
    quickCheck prop_IsRectangular
    putStrLn "Test"
