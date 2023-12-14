module AoC2023.Day14

open System.Threading
open AoC2023.Prelude

type Shape =
    | Round
    | Cube

type Direction = North|South|East|West

type Rock = { Shape: Shape; Row: int; Col: int }

let getShape =
    function
    | 'O' -> Some Round
    | '#' -> Some Cube
    | _ -> None

let getLoad dim rock =
    match rock.Shape with
    | Round -> dim - rock.Row |> int64
    | _ -> 0L

let isInCol col rock =
    rock.Col = col

let isInRow row rock =
    rock.Row = row
    
let getRocksInColumn rocks c =
    let rocks' = rocks |> Seq.filter (isInCol c) |> Seq.sortBy _.Row |> List.ofSeq
    c,rocks'
    
let getRocksInRow rocks r =
    let rocks' = rocks |> Seq.filter (isInRow r) |> Seq.sortBy _.Col |> List.ofSeq
    r,rocks'

let organize direction rocks =
    let cols = rocks |> Seq.map _.Col |> Seq.max
    let rows = rocks |> Seq.map _.Row |> Seq.max
    match direction with
    |North ->
        seq [0..cols] |> Seq.map (getRocksInColumn rocks) |> Map.ofSeq
    |South ->
        seq [0..cols] |> Seq.map (getRocksInColumn rocks) |> Seq.rev |> Map.ofSeq
    |East ->
        seq [0..rows] |> Seq.map (getRocksInRow rocks) |> Map.ofSeq
    |West ->
        seq [0..rows] |> Seq.map (getRocksInRow rocks) |> Seq.rev |> Map.ofSeq
        
let rec rollColumn dir moved remaining =
     let increment =
         match dir with
         |North|East -> 1
         |South|West -> -1
     let getPos =
         match dir with
         |North|South -> (function r -> r.Row)
         |East|West -> (function r -> r.Col)
     match remaining with
         | [] -> moved
         | head :: tail ->
             let row = moved |> Seq.tryLast |> function
                 | Some r -> getPos r + increment
                 | None -> 0 
             let rock =
                 match head.Shape with
                 | Round -> {head with Row=row}
                 | Cube -> head
             rollColumn North (moved @ [rock]) tail
         
let roll dir rockmap =
    rockmap |> Map.map (fun _ rocks -> rollColumn dir List.empty<Rock> rocks)
    
let part1 fn () =
    let input = readInput fn
    let height = Seq.length input
    let rocks =
        input
        |> Seq.mapi (fun row arr ->
            arr
            |> Seq.mapi (fun col c ->
                match getShape c with
                | Some s -> Some { Shape = s; Row = row; Col = col }
                | None -> None))
        |> Seq.concat
        |> Seq.choose id
        |> (organize North)
    
    rocks |> roll North |> Map.values |> Seq.concat |> Seq.map (getLoad height) |> Seq.sum

let part2 fn () = 0L
