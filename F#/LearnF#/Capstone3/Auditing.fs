module Capstone3.Auditing

open Capstone3.Domain


let fileSystemAudit account message =
    let foldername = "/tmp/audits"
    let filename = sprintf "%s/%O.txt" foldername account.AccountId
    if (not (System.IO.Directory.Exists filename)) then
        System.IO.Directory.CreateDirectory foldername |> ignore
    else
        ()
    let m = sprintf "Account %O: %s" account.AccountId message
    System.IO.File.AppendAllText (filename, m)


let consoleAudit account message = 
    let m = sprintf "Account %O: %s" account.AccountId message
    printfn "%s" m
