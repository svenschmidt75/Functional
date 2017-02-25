type Customer = { Name : string }
type Account = { AccountId : System.Guid; Owner : Customer; Balance : decimal }

let deposit amount account = { account with Balance = amount + account.Balance }

let withdraw amount account =
    if amount > account.Balance then
        account
    else
        { account with Balance = account.Balance - amount }


let account = { AccountId = System.Guid.NewGuid (); Owner = { Name = "Sven" }; Balance = 10m }
printfn "%M" (account
             |> deposit 50m
             |> withdraw 25m
             |> deposit 10m).Balance

let fileSystemAudit account message =
    let foldername = "/tmp/audits"
    let filename = sprintf "%s/%O.txt" foldername account.AccountId
    if (not (System.IO.Directory.Exists filename)) then
        System.IO.Directory.CreateDirectory foldername |> ignore
    else
        ()
    let m = sprintf "Account %O: %s" account.AccountId message
    System.IO.File.AppendAllText (filename, m)

fileSystemAudit account "audits\n"


let consoleAudit account message = 
    let m = sprintf "Account %O: %s" account.AccountId message
    printfn "%s" m

consoleAudit account "Hi there"