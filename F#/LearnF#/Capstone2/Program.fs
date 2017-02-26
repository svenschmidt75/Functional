// Learn more about F# at http://fsharp.org

open Capstone2.Operations
open Capstone2.Domain
open Capstone2.Auditing
open System

let getAction () =
    printf "w(ithdraw), d(deposit), e(x)it: "
    Console.ReadLine ()

let getAmount () = 
    printf "What amount: "
    let amountTxt = Console.ReadLine ()
    Decimal.Parse amountTxt

[<EntryPoint>]
let main argv = 
    let customer = { Name = "Isaac" }
    let mutable account = { AccountId = System.Guid.Empty; Owner = customer; Balance = 90M }
    let withdrawWithAudit = withdraw |> auditAs "withdraw" consoleAudit
    let depositWithAudit = deposit |> auditAs "deposit" consoleAudit
    while true do
        let action = getAction ()
        if action = "x" then Environment.Exit 0
        let amount = getAmount ()
        account <-if action = "d" then account |> depositWithAudit amount
                  elif action = "w" then account |> withdrawWithAudit amount
                  else account
    0