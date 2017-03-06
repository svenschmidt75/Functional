#load "Domain.fs"
#load "Operations.fs"
#load "Transaction.fs"
#load "Auditing.fs"
#load "FileRepository.fs"


open System
open Capstone3.Operations
open Capstone3.Domain
open Capstone3.Transaction
open Capstone3.FileReposititory

// let t = [{Timestamp = DateTime.Now; Operation = "withdraw"; Amount = 10M; Accepted = true;}; {Timestamp = DateTime.Now; Operation = "deposit"; Amount = 40M; Accepted = true;}]
// loadAccount "Sven" Guid.Empty t


// let record = "Account 61c35614-c95e-4007-8514-cf1491e7ccb9: 3/5/17 9:45:59 AM***deposit***100***true"

// let parts = record.Split([|":"|], StringSplitOptions.None)
// let p2 = parts.[0].Split([|" "|], StringSplitOptions.None)

// Guid.Parse p2.[1]

//let a = Capstone3.FileReposititory.findTransactionsOnDisk "Sven"

let loadAccountFromDisk ownerName =
    let (accountId, transactions) = findTransactionsOnDisk ownerName
    loadAccount ownerName accountId transactions

let b = loadAccountFromDisk "Sven"