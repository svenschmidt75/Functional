module Transactions
     ( Transaction (..)
     , findTransactionsOnDisk
     , deserialize
     , getGuid
     , writeTransaction
     ) where

import Data.Time.Clock
import Data.Time.Format
import Data.Decimal
import Text.Printf
import qualified System.Directory as SD (doesDirectoryExist
                                       , createDirectoryIfMissing
                                       , listDirectory)
import qualified Data.ByteString as DBS (readFile
                                       , writeFile
                                       , appendFile)
import qualified Data.ByteString.Char8 as DBS8
import qualified Data.UUID as DU
import qualified Data.UUID.V4 as DU4
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
    dirExists <- SD.doesDirectoryExist folderName
    if not dirExists then do
        SD.createDirectoryIfMissing True folderName
        id <- createEmptyTransactionFile folderName
        return (id, [])
    else do
        files <- SD.listDirectory folderName
--        mapM_ putStrLn files
        if null files then do
            id <- createEmptyTransactionFile folderName
            return (id, [])
        else do
            let transactionFile = printf "%s/%s" folderName (head files)
            guid <- getGuid transactionFile
            content <- lines <$> (DBS8.unpack <$> DBS.readFile transactionFile)
            return (guid, map deserialize content)

createEmptyTransactionFile :: String -> IO DU.UUID
createEmptyTransactionFile folderName = do
    id <- DU4.nextRandom
    let transactionFileName = printf "%s/%s.txt" folderName (DU.toString id)
    _ <- DBS.writeFile transactionFileName (DBS8.pack "")
    return id

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
stringToBool "True"  = True
stringToBool "false" = False
stringToBool "False" = False

writeTransaction :: String -> DU.UUID -> Transaction -> IO ()
writeTransaction name accountId transaction = do
    let time                = formatTime defaultTimeLocale "%D %T %P" (timestamp transaction)
        serializeLog        = printf "Account %s: %s***%s***%v***%v\n" (DU.toString accountId) time (operation transaction) (show $ amount transaction) (show $ accepted transaction)
        transactionFileName = printf "/tmp/audits/%s/%s.txt" name (DU.toString accountId)
    DBS.appendFile transactionFileName (DBS8.pack serializeLog)
