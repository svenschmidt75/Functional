module Capstone3.Operations

open System
open Capstone3.Domain


let deposit amount account = { account with Balance = amount + account.Balance }

let withdraw amount account =
    if amount > account.Balance then
        printf "Transaction rejected\n"
        account
    else
        printf "Transaction approved\n"
        { account with Balance = account.Balance - amount }

let auditAs (operationName : string) (audit : Account -> Transaction -> unit) (operation : decimal -> Account -> Account) (amount : decimal) (account : Account) : Account =
    let newAccount = operation amount account
    let transaction : Transaction = { Timestamp = DateTime.Now; Operation = operationName; Amount = amount; Accepted = account.Balance <> newAccount.Balance; }
    audit newAccount transaction
    newAccount

let loadAccount (owner : string) (accountId : System.Guid) (transactions : Transaction list) =
    let processTransaction (account : Account) (transaction : Transaction) =
        if not transaction.Accepted then
            account
        else
            if transaction.Operation = "withdraw" then
                { account with Balance = account.Balance - transaction.Amount }
            elif transaction.Operation = "deposit" then
                { account with Balance = account.Balance + transaction.Amount }
            else
                failwith "Invalid command"
    let initialAccount = { AccountId = accountId; Owner = { Name = owner; }; Balance = 0M; }
    List.fold processTransaction initialAccount transactions
