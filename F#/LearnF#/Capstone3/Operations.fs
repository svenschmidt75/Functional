module Capstone3.Operations

open System
open Capstone3.Domain


let deposit amount account = { account with Balance = amount + account.Balance }

let withdraw amount account =
    if amount > account.Balance then
        printf "Transaction rejected"
        account
    else
        printf "Transaction approved"
        { account with Balance = account.Balance - amount }

let auditAs (operationName : string) (audit : Account -> Transaction -> unit) (operation : decimal -> Account -> Account) (amount : decimal) (account : Account) : Account =
    let newAccount = operation amount account
    //let msg = sprintf "Performed operation `%s` for $%M. New balance is $%M." operationName amount newAccount.Balance
    let transaction : Transaction = { Timestamp = DateTime.Now; Operation = operationName; Amount = amount; Accepted = true; }
    audit newAccount transaction
    newAccount

let loadAccount (owner : string) (accountId : System.Guid) (transactions : Transaction list) =
    let processTransaction (account : Account) (transaction : Transaction) =
        if transaction.Operation = "withdraw" then
            { account with Balance = account.Balance - transaction.Amount }
        elif transaction.Operation = "deposit" then
            { account with Balance = account.Balance + transaction.Amount }
        else
            failwith "Invalid command"
    let initialAccount = { AccountId = accountId; Owner = { Name = owner; }; Balance = 0M; }
    List.fold processTransaction initialAccount transactions
