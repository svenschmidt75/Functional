module Uncertainty where

data Card = Ace
          | Number Integer
          | Picture

valueCard :: Card -> P Integer
valueCard Ace        = [1, 11]
valueCard (Number x) = [x]
valueCard Picture    = [10]

valueHand :: [Card] -> Integer
valueHand cs = let hvs = hv cs
                   nonBust = filter (<= 21) hvs
               in if not (null nonBust)
                    then maximum nonBust
                    else minimum hvs
           where
               hv [] = [0]
               hv (c:cs) = (valueCard c) `addP` hv cs

main = do
    print (valueHand [])
    print (valueHand [Ace, Number 2, Number 3])
    print (valueHand [Ace, Ace, Number 2])
    print (valueHand [Ace, Ace, Ace])
    print (valueHand [Picture, Picture])
    print (valueHand [Ace, Picture])
    print (valueHand [Ace, Picture, Picture])
    print (valueHand [Ace, Picture, Picture, Picture])

type P a = [a]

composeP :: (b -> P c) -> (a -> P b) -> (a -> P c)
--composeP f g x = [ z | y <- g x, z <- f y]
--composeP f g x = h (g x)
--              where
--                h [] = []
--                h (y:ys) = (f y) ++ (h ys)

composeP f g x = concatMap f (g x)
              --where
              --  h [] = []
              --  h (y:ys) = (f y) ++ (h ys)

idP :: a -> P a
idP x = [x]

addP :: P Integer -> P Integer -> P Integer
addP xs ys = [ x + y | x <- xs, y <- ys ]

joinP :: P ( P a ) -> P a
--joinP xss = h (id xss)
--            where
--              h []     = []
--              h (y:ys) = (id y) ++ (h ys)

joinP = composeP id id

bindP :: P a -> (a -> P b) -> P b
bindP e f = (composeP f id) e

