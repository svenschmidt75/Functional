module Capstone3.Auditing

open Capstone3.Domain


let fileSystemAudit account message =
    let foldername = sprintf "/tmp/audits/%s" account.Owner.Name
    if (not (System.IO.Directory.Exists foldername)) then
        System.IO.Directory.CreateDirectory foldername |> ignore
    else
        ()
    let m = sprintf "Account %O: %s\n" account.AccountId message
    let filename = sprintf "%s/%O.txt" foldername account.AccountId
    System.IO.File.AppendAllText (filename, m)


let consoleAudit account (transaction : Transaction) = 
    let m = sprintf "Account %O: %A" account.AccountId transaction
    printfn "%s" m
