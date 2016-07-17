module Poker where

import Data.List (sort, sortBy)


data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Show, Eq, Ord)

data Rank = Number Int
          | Jack
          | Queen
          | King
          | Ace
          deriving (Show, Eq)

instance Ord Rank where
    (Number i1) <= (Number i2) = i1 <= i2
    (Number _)  <= Jack        = True
    (Number _)  <= Queen       = True
    (Number _)  <= King        = True
    (Number _)  <= Ace         = True
    Jack        <= (Number _)  = False
    Queen       <= (Number _)  = False
    King        <= (Number _)  = False
    Ace         <= (Number _)  = False

data Card = Card Suit Rank
          deriving (Show, Eq, Ord)

type Hand = [Card]


--partition :: (Card -> Rank) -> [Hand] -> [[Card]]
--partition _ [] = []
--partition p () = 

partition :: [Int] -> [[Int]]
partition xs = let ordered = sort xs in
               doPartition ordered [] []
               where
                 doPartition []       _       result = result
                 doPartition (x:[])   current result = (x:current) : result
                 doPartition (x:y:xs) current result
                    | x == y    = doPartition (y:xs) (x:current) result
                    | otherwise = doPartition (y:xs) []          result'
                                  where
                                    result' = (x:current) : result

tryHighCard :: Hand -> Maybe Card
tryHighCard xs = Just . head . reverse $ sortBy maxByRank xs
               where
                 maxByRank (Card _ rank1) (Card _ rank2) = rank1 `compare` rank2

--tryPair :: Hand -> Maybe [Card]



main :: IO ()
main = do
    let hand = [(Card Spades (Number 7)), (Card Hearts Ace)]
    print $ tryHighCard hand
    return ()
