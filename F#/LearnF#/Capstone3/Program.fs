// Learn more about F# at http://fsharp.org

open Capstone3.Operations
open Capstone3.Domain
open Capstone3.Auditing
open System


let isValidCommand (command : char) =
    let cmds = ['w'; 'x'; 'd']
    List.contains command cmds

let isStopCommand (command : char) = command = 'x'

let processCommand (account : Account) (command : char, amount : decimal) =
    let withdrawWithAudit = withdraw |> auditAs "withdraw" consoleAudit
    let depositWithAudit = deposit |> auditAs "deposit" consoleAudit
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
let main argv = 
    let customer = { Name = "Isaac" }
    let openingAccount = { AccountId = System.Guid.Empty; Owner = customer; Balance = 90M }
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
