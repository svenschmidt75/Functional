#load "Domain.fs"
#load "Operations.fs"

open Capstone3.Operations
open Capstone3.Domain
open System

let isValidCommand (command : char) =
    let cmds = ['w'; 'x'; 'd']
    List.contains command cmds

let isStopCommand (command : char) = command = 'x'

let getAmount (command : char) = if command = 'd' then
                                    command, 50M
                                 elif command = 'w' then
                                    command, 25M
                                 else
                                    'x', 0M

let processCommand (account : Account) (command : char, amount : decimal) =
    if command = 'w' then
        { account with Balance = account.Balance - amount }
    elif command = 'd' then
        { account with Balance = account.Balance + amount }
    else
        failwith "Invalid command"

let openingAccount = { Owner = { Name = "Isaac" }; Balance = 0M; AccountId = Guid.Empty }
let account =
    let commands = [ 'd'; 'w'; 'z'; 'f'; 'd'; 'x'; 'w' ]
    commands
    |> Seq.filter isValidCommand
    |> Seq.takeWhile (not << isStopCommand)
    |> Seq.map getAmount
    |> Seq.fold processCommand openingAccount
