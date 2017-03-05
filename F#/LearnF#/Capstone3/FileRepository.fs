module Capstone3.FileReposititory

open System
open Capstone3.Domain
open Capstone3.Auditing
open Capstone3.Transaction


let writeTransaction (account : Account) (transaction : Transaction) =
    let serialized transaction =
        sprintf "%O***%s***%M***%b" transaction.Timestamp transaction.Operation transaction.Amount transaction.Accepted
    fileSystemAudit account (serialized transaction)

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
        let files = System.IO.Directory.GetFiles "%s"
        if (Array.isEmpty files) then
            System.Guid.NewGuid (), List.empty
        else
            getAccountId files.[0], System.IO.File.ReadAllLines files.[0]
                                 |> Array.map deserialize
                                 |> Array.toList
