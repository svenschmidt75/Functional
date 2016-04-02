// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.


let collatz (start_value : uint64) =
    let rec collatz_helper (value : uint64) count lst =
        if value = 1UL then
            count, 1UL :: lst
            else
                match value%2UL=0UL with
                    | true -> collatz_helper (value / 2UL) (count + 1) (value :: lst)
                    | false -> collatz_helper (3UL * value + 1UL) (count + 1) (value :: lst)
    let ret = collatz_helper start_value 0 []
    (fst ret, List.rev (snd ret))

[<EntryPoint>]
let main argv = 


    printfn "Number of steps: %A" (collatz 22UL)
    printfn "Number of steps: %A" (collatz 837799UL)
    0 // return an integer exit code