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

[<TailCall>]
let rec roll' (accumulated: Tile seq, remaining: Tile seq) =
    match Seq.length remaining with
    | 0 -> accumulated
    | 1 -> Seq.append accumulated remaining
    | _ ->
        let head = Seq.head remaining
        match head with
        | Cube -> roll' (Seq.append accumulated [head], Seq.tail remaining)
        | _ ->
            let newRolled = Seq.takeWhile ((<>) Cube) remaining |> Seq.sort
            roll' (Seq.append accumulated newRolled, Seq.skip (Seq.length newRolled) remaining)

let roll line =
    roll' (Seq.empty<Tile>, line)
   
// let roll = memoize roll'

let rollWest = Seq.map roll
let rollEast = Seq.map (Seq.rev >> roll >> Seq.rev)
let rollNorth = Seq.transpose >> Seq.map roll >> Seq.transpose
let rollSouth = Seq.transpose >> Seq.map (Seq.rev >> roll >> Seq.rev) >> Seq.transpose

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
    let rocks = input |> Seq.map (Seq.map getShape)
    spin rounds 0 rocks |> printMap
    0L

let part1 fn () =
    solve 1 fn ()
    
let part2 fn () =
    solve 100_000_000 fn ()
