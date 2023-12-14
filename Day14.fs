module AoC2023.Day14

open System.Diagnostics
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
    rocks |> Array.iter (fun r -> String.concat "" (r |> Array.map getSymbol) |> printfn "%s")

[<TailCall>]
let rec roll' (accumulated: Tile array, remaining: Tile array) =
    match Array.length remaining with
    | 0 -> accumulated
    | 1 -> Array.append accumulated remaining
    | _ ->
        let head = Array.head remaining
        match head with
        | Cube -> roll' (Array.append accumulated [|head|], Array.tail remaining)
        | _ ->
            let newRolled = Array.takeWhile ((<>) Cube) remaining |> Array.sort
            roll' (Array.append accumulated newRolled, Array.skip (Array.length newRolled) remaining)

let roll line =
    roll' (Array.empty<Tile>, line)
   
// let roll = memoize roll'

let rollWest = Array.map roll
let rollEast = Array.map (Array.rev >> roll >> Array.rev)
let rollNorth = Array.transpose >> Array.map roll >> Array.transpose
let rollSouth = Array.transpose >> Array.map (Array.rev >> roll >> Array.rev) >> Array.transpose

let spinOnce rocks =
    rocks |> rollNorth |> rollWest |> rollSouth |> rollEast
    

let spin until rounds rocks =
    let progress = until / 100
    let stopwatch = Stopwatch()
    stopwatch.Start()
    
    let rec spinInternal rounds rocks =
        if rounds = until then 
            rocks
        else
            let rocks' = spinOnce rocks
            if rounds % progress = 0 then 
                stopwatch.Stop()
                printfn "%d%% done, lap time: %A" (rounds/progress) stopwatch.Elapsed
                stopwatch.Restart() 
            spinInternal (rounds + 1) rocks'
    spinInternal rounds rocks
     
let solve rounds fn () =
    let input = readInput fn
    let rocks = input |> Array.ofSeq |> Array.map (Seq.map getShape >> Array.ofSeq)
    spin rounds 0 rocks |> printMap
    0L

let part1 fn () =
    solve 1 fn ()
    
let part2 fn () =
    solve 100_000_000 fn ()
