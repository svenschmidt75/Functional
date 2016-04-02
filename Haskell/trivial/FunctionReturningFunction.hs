
type Config = Integer

type Pref a = Config -> a

f :: Integer -> Pref Integer
f a = \c -> a

-- f 10 :: Pref Integer
-- f 10 5 :: Integer
