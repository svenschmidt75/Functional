module Capstone2.Operations

open Capstone2.Domain


let deposit amount account = { account with Balance = amount + account.Balance }

let withdraw amount account =
    if amount > account.Balance then
        account
    else
        { account with Balance = account.Balance - amount }

let auditAs (operationName : string) (audit : Account -> string -> unit) (operation : decimal -> Account -> Account) (amount : decimal) (account : Account) : Account =
    let newAccount = operation amount account
    let msg = sprintf "Performed operation `%s` for $%M. New balance is $%M." operationName amount newAccount.Balance
    audit newAccount msg
    newAccount
