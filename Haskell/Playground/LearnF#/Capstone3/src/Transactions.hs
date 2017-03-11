module Transactions
     ( Transaction
     , findTransactionsOnDisk
     , deserialize
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


data Transaction = Transaction { timestamp :: UTCTime
                               , operation :: String
                               , amount    :: Decimal
                               , accepted  :: Bool
                               }
    deriving (Show, Eq)


findTransactionsOnDisk :: String -> IO [Transaction]
findTransactionsOnDisk name = do
    let folderName = printf "/tmp/audits/%s" name
    dirExists <- doesDirectoryExist folderName
    if not dirExists then
        return []
    else do
        files <- listDirectory folderName
        mapM_ putStrLn files
        if null files then
            return []
        else do
            -- get account id from file name
            -- read file line-by-line
            -- turn lines into transactions
            let transactionFile = printf "%s/%s" folderName (head files)
            guid <- getGuid transactionFile
            content <- readFile transactionFile
            let lineContent = lines content
            return [] -- $ getAccountId (head files)
    -- where
    --     getAccountId fileName =

getGuid :: FilePath -> IO (Maybe DU.UUID)
getGuid fileName = do
    putStrLn fileName
    content <- readFile fileName
    let l = lines content
    mapM_ putStrLn l
    return $ DU.fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

deserialize :: String -> Transaction
deserialize content = Transaction datetime operation amount accepted
    where
        split1 = splitOn "***" content
        operation = split1 !! 1
        amount = (read (split1 !! 2)) :: Decimal
        accepted = stringToBool (split1 !! 3)
        split2 = splitOn " " (head split1)
        datetimeStr = concat [split2 !! 2, " ", split2 !! 3, " ", split2 !! 4]
        datetime = parseTimeOrError True defaultTimeLocale "%-m/%-d/%y %-I:%-M:%-S %P" datetimeStr

stringToBool :: String -> Bool
stringToBool "true"  = True
stringToBool "false" = False