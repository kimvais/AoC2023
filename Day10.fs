module AoC2023.Day10

open AoC2023.Prelude

type Connection = North|East|South|West
type Pipe = NorthSouth|NorthEast|NorthWest|WestSouth|EastSouth|EastWest|Ground|Start
let getConnections = function
    | NorthSouth -> [true;false;true;false]
    | NorthEast -> [true;true;false;false]
    | NorthWest -> [true;false;false;true]
    | WestSouth -> [false;false;true;true]
    | EastSouth -> [false;true;true;false]
    | EastWest -> [false;true;false;true]
    | Ground -> [false;false;false;false]

let getSymbol = function
    |NorthSouth -> "\u2551"
    |NorthEast -> "\u255A"
    |NorthWest -> "\u255d"
    |WestSouth -> "\u2557"
    |EastSouth -> "\u2554"
    |EastWest -> "\u2550"
    |Start -> "\U0001F42D"
    |Ground -> " "
    
let makePipe = function
    | '|' -> NorthSouth
    | 'L' -> NorthEast
    | '-' -> EastWest
    | 'J' -> NorthWest
    | '7' -> WestSouth
    | 'F' -> EastSouth
    | 'S' -> Start
    |'.' -> Ground
    
let printRow row =
   row |> Array.iter (makePipe >> getSymbol >> printf "%s")
   printfn ""
   

let part1 fn () =
    let rows = readInput fn |> Array.ofSeq |> Array.map Array.ofSeq
    rows |> Array.iter printRow
    
    
    0L

let part2 fn () = 0L