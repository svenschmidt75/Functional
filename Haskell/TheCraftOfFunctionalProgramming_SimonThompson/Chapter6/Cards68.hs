module Main where

import Data.List


data Suit = Spades
          | Hearts
          | Diamonds
          | Clubs
          deriving (Eq, Show)

type Value = Int

data Card = Card Suit Value
          | Jack Suit
          | Queen Suit
          | King Suit
          | Ace Suit
          deriving (Show)

type Deck = [Card]

data Player = North
            | East
            | South
            | West
            deriving (Show)

type Trick = [(Player, Card)]

cardValue :: Card -> Value
cardValue (Card _ x) = x
cardValue (Jack _)   = 12
cardValue (Queen _)  = 13
cardValue (King _)   = 14
cardValue (Ace _)    = 15

cardSuit :: Card -> Suit
cardSuit (Card s _) = s
cardSuit (Jack s)   = s
cardSuit (Queen s)  = s
cardSuit (King s)   = s
cardSuit (Ace s)    = s

getLeadPlayer :: Trick -> Player
getLeadPlayer []          = error "Empty trick"
getLeadPlayer ((x, _):_) = x

-- win, no trump
-- the 1st element is the lead
winNT :: Trick -> Player
winNT ((x, y):xs) = let leader      = x in
                    let leader_card = y in
                    let max_player = sortBy (\(_, card1) -> \(_, card2) -> sortCards card1 card2) $ filter (\(_, card) -> compareCard card leader_card) xs in
                      if null max_player then leader else (getLeadPlayer max_player)
                      where
                        compareCard card1 card2 = cardValue card1 > cardValue card2 && cardSuit card1 == cardSuit card2
                        sortCards card1 card2 = compare (negate (cardValue card1)) (negate (cardValue card2))

main = do
    let trick = [(East, (Jack Hearts)),
                 (South, (Card Hearts 2)),
                 (West, (Card Clubs 10)),
                 (North, (King Hearts))]
    print $ winNT trick
    print $ winNT2 trick
