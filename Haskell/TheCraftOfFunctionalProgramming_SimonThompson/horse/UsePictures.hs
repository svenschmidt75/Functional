module UsePictures where

import Pictures


-- Chapter 2, Exc. 2.2, page 37


whiteSquare :: Picture
whiteSquare = ["..", ".."]

blackSquare :: Picture
blackSquare = ["##", "##"]

exercise22_1 :: Picture
exercise22_1 = beside (above whiteSquare blackSquare) (above blackSquare whiteSquare)

exercise22_2 :: Picture
exercise22_2 = above (beside whiteSquare blackSquare) (beside blackSquare whiteSquare)

chessBoardRow :: Picture
chessBoardRow = beside (beside whiteBlack whiteBlack) (beside whiteBlack whiteBlack)
                where
                    whiteBlack = beside whiteSquare blackSquare

chessBoard :: Picture
chessBoard = let half = above (above chessBoardRow chessBoardRow) (above chessBoardRow chessBoardRow) in
            above half half

exercise23_1 :: Picture
exercise23_1 = above (beside horseV horseVInverted) (beside horseVInverted horseV)
            where
                horseV = flipV horse
                horseVInverted = invertColor horseV

exercise23_2 :: Picture
exercise23_2 = above (beside horseV horseVInverted) (beside horseInverted horse)
            where
                horseV = flipV horse
                horseVInverted = invertColor horseV
                horseInverted = invertColor horse


exercise23_3 :: Picture
exercise23_3 = above (beside horseV horseVInverted) (beside horseHnverted horseH)
            where
                horseV = flipV horse
                horseVInverted = invertColor horseV
                horseH = flipH horse
                horseHnverted = invertColor horseH

exercise24 :: Picture
exercise24 = above (beside horseV horseVInverted) (beside (flipH horseVInverted) (flipH horseV))
            where
                horseV = flipV horse
                horseVInverted = invertColor horseV
