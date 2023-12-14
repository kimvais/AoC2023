module AoC2023.Day14

open System.Threading
open AoC2023.Prelude

type Tile =
    | Round
    | Cube
    | Empty

type Direction = North|South|East|West
let dirMap = [North;West;South;East]

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
    printfn ""
    
let rec recRoll rolled remaining =
    match remaining with
    | [] -> rolled
    | [x] -> rolled @ [x]
    | head::rest ->
        match head with
        | Cube -> recRoll (rolled @ [head]) rest
        | _ ->
            let newRolled = remaining |> List.takeWhile ((<>) Cube) |> List.sort
            recRoll (rolled @ newRolled) (remaining |> List.skip (List.length newRolled))
            
let roll' line =
    recRoll List.empty<Tile> line
   
let roll = memoize roll'

let rec spinOnce dirs rocks =
    match dirs with
    | [] -> rocks
    | dir::rem ->
        let rocks' =
            match dir with
            | West -> rocks |> List.map roll
            | East -> rocks |> List.map (List.rev >> roll >> List.rev)
            | North -> rocks |> List.transpose |> List.map roll |> List.transpose
            | South -> rocks |> List.transpose |> List.map (List.rev >> roll >> List.rev) |> List.transpose
        spinOnce rem rocks'
   
let rec spin until rounds rocks =
    if rounds % 100_000 = 0 then printfn "%d" rounds
    match rounds with
    | r when r = until -> rocks
    | r ->
        let rocks' = spinOnce dirMap rocks
        // printMap rocks'
        spin until (r + 1) rocks'
        
let solve rounds fn () =
    let input = readInput fn
    let rocks = input |> Seq.map (Seq.map getShape >> List.ofSeq) |> List.ofSeq
    spin rounds 0 rocks
    0L

let part1 fn () =
    solve 1 fn ()
    
let part2 fn () =
    solve 1_000_000 fn ()
