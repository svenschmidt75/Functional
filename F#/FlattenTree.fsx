// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.
// Define your library scripting code here
module Script

#load "Library1.fs"

open Library3


type Tree<'a> =
    | Leaf of 'a
    | Branch of Tree<'a> * 'a * Tree<'a>

let l3 = Branch (Leaf 1, 3, Leaf 2)
let r3 = Branch (Leaf 9, 7, Leaf 15)
let top = Branch (l3, 5, r3)

let fridge t =
    let rec fr lst t = 
        match t with
            | Leaf c -> c :: lst
            | Branch (l, c, r) -> 
                let llst = fr lst l
                let rlst = fr lst r
                llst @ rlst @ [c]
    fr [] t

fridge top
