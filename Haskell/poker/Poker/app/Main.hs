module Main where

import Lib

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
    King        <= Queen       = False
    Queen       <= Jack        = False

data Card = Card Suit Rank
          deriving (Show, Eq, Ord)

type Hand = [Card]

maxByRank :: Card -> Card -> Ordering
maxByRank (Card _ rank1) (Card _ rank2) = rank1 `compare` rank2

maxByRankAceLow :: Card -> Card -> Ordering
maxByRankAceLow c1@(Card _ rank1) c2@(Card _ rank2)
    | rank1 == rank2 = EQ
    | rank1 == Ace   = LT
    | rank2 == Ace   = GT
    | otherwise      = maxByRank c1 c2

partitionBy :: Ord b => (a -> b) ->  [a] -> [[a]]
partitionBy p [] = []
partitionBy p xs = let ordered = sortBy (\x y -> p x `compare` p y) xs in
                     doPartitionBy ordered [] []
                       where
                         doPartitionBy []       _       result = result
                         doPartitionBy (x:[])   current result = (x:current):result
                         doPartitionBy (x:y:xs) current result
                           | p x == p y = doPartitionBy (x:xs) (y:current) result
                           | otherwise  = doPartitionBy (y:xs) []          result'
                           where
                             result' = (x:current):result

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

tryCard :: Int -> Hand -> Maybe [Card]
tryCard n deck = let partitionedByRank = partitionBy (\(Card _ rank) -> rank) deck in
                  let pairs = filter (\x -> length x == n) partitionedByRank in
                    case pairs of
                      []     -> Nothing
                      (x:xs) -> Just x

tryHighCard :: Hand -> Maybe [Card]
tryHighCard = tryCard 1

tryPair :: Hand -> Maybe [Card]
tryPair = tryCard 2

tryThreeOfAKind :: Hand -> Maybe [Card]
tryThreeOfAKind = tryCard 3

tryFourOfAKind :: Hand -> Maybe [Card]
tryFourOfAKind = tryCard 4

rankToInt :: Card -> Int
rankToInt (Card _ (Number x)) = x
rankToInt (Card _ Ace) = 1
rankToInt (Card _ Jack) = 11
rankToInt (Card _ Queen) = 12
rankToInt (Card _ King) = 13

-- aces rank lowest
tryStraight :: Hand -> Maybe [Card]
tryStraight deck = let orderedByRank = sortBy maxByRankAceLow deck in
                   let nrf = rankToInt . head $ orderedByRank in
                   let nrl = rankToInt . last $ orderedByRank in
                   if nrl - nrf == 4
                     then Just deck
                     else Nothing

tryFlush :: Hand -> Maybe [Card]
tryFlush hand
    | length (partitionBy (\(Card suit _) -> suit) hand) == 1 = Just hand
    | otherwise                                               = Nothing

main :: IO ()
main = do
    let hand = [
                (Card Diamonds (Number 8)),
                (Card Diamonds (Number 9)),
                (Card Diamonds (Number 10)),
                (Card Diamonds Queen),
                (Card Diamonds Jack)
               ]
    let orderedByRank = sortBy maxByRankAceLow hand
    print orderedByRank
    let nrf = rankToInt . head $ orderedByRank
    print nrf
    let nrl = rankToInt . last $ orderedByRank
    print nrl
    print $ tryStraight hand
    print $ tryFlush hand
    return ()
