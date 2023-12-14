module AoC2023.Day14

open System.Threading
open AoC2023.Prelude

type Tile =
    | Round
    | Cube
    | Empty

type Direction = North|South|East|West

let getShape =
    function
    | 'O' -> Round
    | '#' ->  Cube
    | _ -> Empty

let getSymbol =
    function
        | Round -> "O"
        | Cube -> "#"
        | Empty -> "."
        
let printMap rocks =
    rocks |> Seq.iter (fun r -> String.concat "" (r |> Seq.map getSymbol) |> printfn "%s")
    printfn ""
    
let rec recRoll rolled remaining =
    match Seq.length remaining with
    | 0 -> rolled
    | 1 -> Seq.append rolled remaining
    | _ ->
        let head = Seq.head remaining
        match head with
        | Cube -> recRoll (Seq.append rolled [head]) (Seq.tail remaining)
        | _ ->
            let newRolled = remaining |> Seq.takeWhile ((<>) Cube) |> Seq.sort
            recRoll (Seq.concat [rolled; newRolled]) (remaining |> Seq.skip (Seq.length newRolled))
            
let roll' line =
    recRoll Seq.empty<Tile> line
   
let roll = memoize roll'

let rollWest = Seq.map roll
let rollEast = Seq.map (Seq.rev >> roll >> Seq.rev)
let rollNorth = Seq.transpose >> Seq.map roll >> Seq.transpose
let rollSouth = Seq.transpose >> Seq.map (Seq.rev >> roll >> Seq.rev) >> Seq.transpose

let spinOnce rocks =
    rocks |> rollNorth |> rollWest |> rollSouth |> rollEast

let rec spin until rounds rocks =
    if rounds % 1_000_000 = 0 then printfn "%d %% done" rounds
    match rounds with
    | r when r = until -> rocks
    | r ->
        let rocks' = spinOnce rocks
        // printMap rocks'
        spin until (r + 1) rocks'
        
let solve rounds fn () =
    let input = readInput fn
    let rocks = input |> Seq.map (Seq.map getShape)
    spin rounds 0 rocks |> printMap
    0L

let part1 fn () =
    solve 1 fn ()
    
let part2 fn () =
    solve 100_000_000 fn ()
