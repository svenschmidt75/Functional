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

