// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let fizzbuzz x = match (x % 3 = 0, x % 5 = 0) with
        | (true, true)  -> "fizzbuzz"
        | (true, false) -> "fizz"
        | (false, true) -> "buzz"
        | _             -> string x

[<EntryPoint>]
let main argv = 
    [1..15] |> List.map (fun x -> printfn "%A" (fizzbuzz x))  |> ignore
    0 // return an integer exit code
