module Capstone

open System


/// Gets the distance to a given destination
let getDistance (destination) =
    if destination = "Gas" then 10
    elif destination = "Home" then 25
    elif destination = "Stadium" then 25
    elif destination = "Office" then 50
    else failwith "Unknown destination!"

let calculateRemainingPetrol(currentPetrol : int, distance : int) : int =
    let remaining = currentPetrol - distance
    if remaining < 0 then
        failwith "Ooops! You’ve run out of petrol!"
    else
        remaining

let driveTo (petrol : int, destination : string) : int =
    let dist = getDistance destination
    let newPetrol = calculateRemainingPetrol(petrol, dist)
    if destination = "Gas" then
        newPetrol + 50
    else
        newPetrol

// let a = driveTo(100, "Office")
// let b = driveTo(a, "Stadium")
// let c = driveTo(b, "Gas")
// let answer = driveTo(c, "Home")

let getDestination () =
    Console.Write "Where do you want to go to: "
    Console.ReadLine ()


[<EntryPoint>]
let main argv =
    let mutable petrol = 100;
    while true do
        try
            let destination = getDestination ()
            printfn "Trying to drive to %s" destination
            petrol <- driveTo(petrol, destination)
            printfn "Made it to %s! You have %d petrol left" destination petrol
        with
            ex -> printfn "ERROR: %s" ex.Message
    0 // return an integer exit code
