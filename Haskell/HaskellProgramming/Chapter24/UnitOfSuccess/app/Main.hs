module Main where

import Text.Trifecta

main :: IO ()
main = do
-- Success 123
    print $ parseString (integer >>= \x -> do
        eof
        return x) mempty "123"

-- Failure (interactive):1:4: error: expected: digit,
-- end of input
-- 123abc<EOF>
--    ^
    print $ parseString (integer >>= \x -> do
        eof
        return x) mempty "123abc"

