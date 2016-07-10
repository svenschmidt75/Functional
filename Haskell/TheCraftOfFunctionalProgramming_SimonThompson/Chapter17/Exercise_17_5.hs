module Parser (token, (>*>)) where

import Data.Char

{- Type constructor for parsing.
 - a is the input type, b is the type of the object that the
 - parser creates.
 - We return a list to allow for ambiguous parses, and an empty
 - list signals that no parse applied.
 - The parser returns [a] for the remaining output, which is
 - different for each parse.
 - Most often, a = Char
 - 
 - Notice that this is basically the same as 'State' in the
 - state monad. Here, we thread the remaining content to parse
 - through as state!

 - [a]: Input to parse
 - [(b, [a])] - list of parses (parses object, remaining input)
 -}
type Parse a b = [a] -> [(b, [a])]

{- All these parsers work like this: The only thing they do is to
 - check whether the current input matches a char that we pass in.
 - If it does, we advance the input by one character, otherwise
 - we do not advance the input.
-}


data Expr = Lit Int
          | Var Char
          | Op Ops Expr Expr
          deriving (Show)

data Ops = Add
         | Sub
         | Mul 
         | Div
         | Mod
         deriving (Show)

makeVar :: Char -> Expr
makeVar c = Var c


-- recognize a single a (a single Char)
token :: Eq a => a -> Parse a a
token t (x:xs)
    | t == x    = [(t, xs)]
    | otherwise = []
token _ []      = []

-- apply the result of the 1st parser to the second
infixr 5 >*>
(>*>) :: Parse a b -> Parse a c -> Parse a (b, c)
(>*>) p1 p2 input = [((y, z), rem2) | (y, rem1) <- p1 input, (z, rem2) <- p2 rem1]

-- parser, that uses a predicate to determine whether to advance
-- the input
-- usage: spot (\x -> isLetter x) "Test" = [('T', "est")]
spot :: (a -> Bool) -> Parse a a
spot p (x:xs)
    | p x       = [(x, xs)]
    | otherwise = []
spot _ []       = []

-- return a new parser, that applies a function to the token the input parser
-- recognizes
-- usage: (build . spot) isDigit digitToInt $ "7" = [(7, "")]
build :: Parse a b -> (b -> c) -> Parse a c
build p f input = [(f b, rem) | (b, rem) <- p input]


-- this parser puts a character in the standard context which is a parser
succeed :: b -> Parse a b
succeed b input = [(b, input)]

-- parses either one, i.e. combines both their results
alt :: Parse a b -> Parse a b -> Parse a b
alt p1 p2 input = p1 input ++ p2 input

-- recognize 0 or any number of patterns
-- usage: list (spot isDigit)  "123a" = [("","123a"),("1","23a"),("12","3a"),("123","a")]
-- :t (:) => (:) :: a -> [a] -> [a]
-- :t uncurry (:) => (a, [a]) -> [a]
-- (succeed []): a list can be empty
-- (p >*> list p): recognizes (x, xs)
-- need to turn (x, xs) into x:xs, so uncurry (:)
list :: Parse a b -> Parse a [b]
list p = (succeed []) `alt` ((p >*> list p) `build` (uncurry (:)))

-- Exercise 17.11
-- recognize 0 or at most a fixed number of patterns
-- usage: listn 2 (spot isDigit)  "123456a" = [("","123456a"),("1","23456a"),("12","4563a")]
listn :: Int -> Parse a b -> Parse a [b]
listn 0 _ = succeed []
listn n p = (succeed []) `alt` ((p >*> listn (n - 1) p) `build` (uncurry (:)))

-- Exercise 17.10
-- recognize 0 or 1 pattern
optional :: Parse a b -> Parse a [b]
optional p = listn 1 p

-- Exercise 17.10
-- recognize 0 or more patterns
nelist :: Parse a b -> Parse a [b]
nelist p = list p

parser :: Parse Char Expr
parser = litParse `alt` varParse `alt` opExpParse

varParse :: Parse Char Expr
varParse = spot isVar `build` makeVar

isVar :: Char -> Bool
isVar x = ('a' <= x && x <= 'z')

-- An operator expression will consist of two expressions joined by an operator,
-- the whole construct between a matching pair of parantheses
--opExpParse :: Parse
opExpParse :: Parse Char Expr
opExpParse = (token '(' >*>
              parser    >*>
              spot isOp >*>
              parser    >*>
              token ')')
              `build` makeExpr

-- The _ are placeholders for ( and )
makeExpr :: (Char, (Expr, (Char, (Expr, Char)))) -> Expr
makeExpr (_, (e1, (bop, (e2, _)))) = Op (charToOp bop) e1 e2

litParse :: Parse Char Expr
litParse = ((optional (token '~')) >*>
            (nelist (spot isDigit)))
            `build` (charlistToExpr . uncurry func)

func :: [Char] -> [Char] -> [Char]
func c1 c2 = concat [c1, c2]

-- exercise 17.12
isOp :: Char -> Bool
isOp '+' = True
isOp '-' = True
isOp '*' = True
isOp '/' = True
isOp '%' = True
isOp _   = False

-- exercise 17.12
charToOp :: Char -> Ops
charToOp '+' = Add
charToOp '-' = Sub
charToOp '*' = Mul
charToOp '/' = Div
charToOp '%' = Mod

charlistToExpr :: [Char] -> Expr
-- charListToExpr "234" -> Lit 234
-- charListToExpr "~98" -> Lit (-98)
charlistToExpr (x:xs)
    | x == '~'  = Lit $ (- read xs)
    | otherwise = Lit $ read (x:xs)

