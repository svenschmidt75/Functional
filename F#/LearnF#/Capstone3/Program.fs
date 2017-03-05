// Learn more about F# at http://fsharp.org

open System
open Capstone3.Operations
open Capstone3.Domain
open Capstone3.Auditing
open Capstone3.FileReposititory


let loadAccountFromDisk ownerName =
    let (accountId, transactions) = findTransactionsOnDisk ownerName
    loadAccount ownerName accountId transactions

let isValidCommand (command : char) =
    ['w'; 'x'; 'd'] |> List.contains command

let isStopCommand = (=) 'x'

let processCommand (account : Account) (command : char, amount : decimal) =
    let withdrawWithAudit = withdraw |> auditAs "withdraw" writeTransaction
    let depositWithAudit = deposit |> auditAs "deposit" writeTransaction
    if command = 'w' then
        withdrawWithAudit amount account
    elif command = 'd' then
        depositWithAudit amount account
    else
        failwith "Invalid command"

let getAmountConsole (command : char) =
    printf "\nWhat amount: "
    command, Console.ReadLine() |> Decimal.Parse

let consoleCommands = seq {
    while true do
        Console.Write "(d)eposit, (w)ithdraw or e(x)it: "
        yield Console.ReadKey().KeyChar
}

[<EntryPoint>]
let main _ = 
    let openingAccount =
        Console.Write "Please enter your name: "
        Console.ReadLine() |> loadAccountFromDisk
    let closingAccount =
        consoleCommands
        |> Seq.filter isValidCommand
        |> Seq.takeWhile (not << isStopCommand)
        |> Seq.map getAmountConsole
        |> Seq.fold processCommand openingAccount
    printfn ""
    printfn "Closing Balance:\r\n %A" closingAccount
    Console.ReadKey() |> ignore
    0
