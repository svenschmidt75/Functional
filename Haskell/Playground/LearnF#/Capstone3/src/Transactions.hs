module Transactions
     ( Transaction
     , findTransactionsOnDisk
     )
 where

import Data.Time.Clock
import Data.Decimal
import Text.Printf
import System.Directory
import System.IO
import qualified Data.UUID as DU


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
            _ <- getGuid (printf "%s/%s" folderName (head files))
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



{-

let findTransactionsOnDisk (ownerName : string) : (System.Guid * Transaction list) =
    let getAccountId filename =
        let content = System.IO.File.ReadAllLines filename
        let parts = content.[0].Split([|":"|], StringSplitOptions.None)
        let p2 = parts.[0].Split([|" "|], StringSplitOptions.None)
        Guid.Parse p2.[1]
    let foldername = sprintf "/tmp/audits/%s" ownerName
    if (not (System.IO.Directory.Exists foldername)) then
        System.Guid.NewGuid (), List.empty
    else
        let files = System.IO.Directory.GetFiles  foldername
        if (Array.isEmpty files) then
            System.Guid.NewGuid (), List.empty
        else
            getAccountId files.[0], System.IO.File.ReadAllLines files.[0]
                                 |> Array.map deserialize
                                 |> Array.toList

-}