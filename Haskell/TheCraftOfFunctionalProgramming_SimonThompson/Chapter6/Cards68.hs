module Main where

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


-- win, no trump
-- the 1st element is the lead
winNT :: Trick -> Player
winNT ((x, y):xs) = let leader      = x in
                    let leader_card = y in
                     let max_player = filter (\(player, card) -> cardValue card > cardValue leader_card && cardSuit card == cardSuit leader_card) xs in
                     if null max_player then leader else (fst (head max_player))

main = do
    let trick = [(East, (Jack Hearts)),
                 (South, (Card Hearts 2)),
                 (West, (Card Clubs 10)),
                 (North, (King Hearts))]
    print $ winNT trick