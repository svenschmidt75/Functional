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

maxByRank :: Card -> Card -> Ordering
maxByRank (Card _ rank1) (Card _ rank2) = rank1 `compare` rank2

partitionBy :: Hand -> [[Card]]
partitionBy []    = []
partitionBy cards = let ordered = sortBy maxByRank cards in
                      doPartitionBy ordered [] []
                        where
                          doPartitionBy []       _       result = result
                          doPartitionBy (x:[])   current result = (x:current):result
                          doPartitionBy (c1@(Card _ rank1):c2@(Card _ rank2):xs) current result
                            | rank1 == rank2 = doPartitionBy (c2:xs) (c1:current) result
                            | otherwise      = doPartitionBy (c2:xs) []           result'
                            where
                              result' = (c1:current) : result

partitionBy2 :: Ord b => (a -> b) ->  [a] -> [[a]]
partitionBy2 p []    = []
partitionBy2 p xs = let ordered = sortBy (\x y -> p x `compare` p y) xs in
                      doPartitionBy ordered [] []
                        where
                          doPartitionBy []       _       result = result
                          doPartitionBy (x:[])   current result = (x:current):result
                          doPartitionBy (x:y:xs) current result
                            | p x == p y = doPartitionBy (x:xs) (y:current) result
                            | otherwise      = doPartitionBy (y:xs) []           result'
                            where
                              result' = (x:current) : result



{-
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
-}

tryHighCard :: Hand -> Maybe Card
tryHighCard xs = Just . head . reverse $ sortBy maxByRank xs

--tryPair :: Hand -> Maybe [Card]



main :: IO ()
main = do
    let hand = [(Card Spades (Number 7)), (Card Hearts Ace), (Card Diamonds (Number 7))]
    print $ tryHighCard hand
    return ()
