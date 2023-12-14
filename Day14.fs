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
    rocks |> List.iter (fun r -> String.concat "" (r |> List.map getSymbol) |> printfn "%s")

[<TailCall>]
let rec roll' (accumulated: Tile list, remaining: Tile list) =
    match List.length remaining with
    | 0 -> accumulated
    | 1 -> List.append accumulated remaining
    | _ ->
        let head = List.head remaining
        match head with
        | Cube -> roll' (List.append accumulated [head], List.tail remaining)
        | _ ->
            let newRolled = List.takeWhile ((<>) Cube) remaining |> List.sort
            roll' (List.append accumulated newRolled, List.skip (List.length newRolled) remaining)

let roll line =
    roll' (List.empty<Tile>, line)
   
// let roll = memoize roll'

let rollWest = List.map roll
let rollEast = List.map (List.rev >> roll >> List.rev)
let rollNorth = List.transpose >> List.map roll >> List.transpose
let rollSouth = List.transpose >> List.map (List.rev >> roll >> List.rev) >> List.transpose

let spinOnce rocks =
    rocks |> rollNorth |> rollWest |> rollSouth |> rollEast
    
[<TailCall>]
let rec spin until rounds rocks =
    if rounds = until then 
        rocks
    else
        let rocks' = spinOnce rocks
        if rounds % 1_000_000 = 0 then printfn "%d %% done" (rounds + 1)
        spin until (rounds + 1) rocks'
 
let solve rounds fn () =
    let input = readInput fn
    let rocks = input |> List.ofSeq |> List.map (Seq.map getShape >> List.ofSeq)
    spin rounds 0 rocks |> printMap
    0L

let part1 fn () =
    solve 1 fn ()
    
let part2 fn () =
    solve 100_000_000 fn ()
