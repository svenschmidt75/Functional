module Transactions
     ( Transaction
     , findTransactionsOnDisk
     , deserialize
     , getGuid
     )
 where

import Data.Time.Clock
import Data.Time.Format
import Data.Decimal
import Text.Printf
import System.Directory
import System.IO
import qualified Data.UUID as DU
import Data.List.Split
import Data.Maybe (fromJust)


data Transaction = Transaction { timestamp :: UTCTime
                               , operation :: String
                               , amount    :: Decimal
                               , accepted  :: Bool
                               }
    deriving (Show, Eq)


findTransactionsOnDisk :: String -> IO (DU.UUID, [Transaction])
findTransactionsOnDisk name = do
    let folderName = printf "/tmp/audits/%s" name
    dirExists <- doesDirectoryExist folderName
    if not dirExists then
        return (DU.nil, [])
    else do
        files <- listDirectory folderName
--        mapM_ putStrLn files
        if null files then
            return (DU.nil, [])
        else do
            let transactionFile = printf "%s/%s" folderName (head files)
            guid <- getGuid transactionFile
            content <- lines <$> readFile transactionFile
            return (guid, map deserialize content)

getGuid :: FilePath -> IO DU.UUID
getGuid fileName = do
    content <- lines <$> readFile fileName
    return $ guid (head content)
    where
        split1         = splitOn ":"
        split2 content = splitOn " " (head $ split1 content)
        guid content   = fromJust $ DU.fromString (split2 content !! 1)

deserialize :: String -> Transaction
deserialize content = Transaction timestamp operation amount accepted
    where
        split1       = splitOn "***" content
        operation    = split1 !! 1
        amount       = read (split1 !! 2)
        accepted     = stringToBool (split1 !! 3)
        split2       = splitOn " " (head split1)
        date         = split2 !! 2
        time         = split2 !! 3
        pm_am        = split2 !! 4
        timestampStr = concat [date, " ", time, " ", pm_am]
        timestamp    = parseTimeOrError True defaultTimeLocale "%-m/%-d/%y %-I:%-M:%-S %P" timestampStr

stringToBool :: String -> Bool
stringToBool "true"  = True
stringToBool "false" = False