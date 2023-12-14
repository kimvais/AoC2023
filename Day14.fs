module AoC2023.Day14

open System.Threading
open AoC2023.Prelude

type Shape =
    | Round
    | Cube

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

let getRocksInColumn rocks c =
    let rocks' = rocks |> Seq.filter (fun r -> r.Col = c) |> Seq.sortBy _.Row |> List.ofSeq
    c,rocks'

let columnize cols rocks =
    seq [0..(cols - 1)] |> Seq.map (getRocksInColumn rocks) |> Map.ofSeq
    
let rec rollColumnNorth moved remaining =
     match remaining with
         | [] -> moved
         | head :: tail ->
             let row = moved |> Seq.tryLast |> function
                 | Some r -> r.Row
                 | None -> -1
             let rock =
                 match head.Shape with
                 | Round -> {head with Row=row + 1}
                 | Cube -> head
             rollColumnNorth (moved @ [rock]) tail
         
let rollNorth rockmap =
    rockmap |> Map.map (fun _ rocks -> rollColumnNorth List.empty<Rock> rocks)
    
let part1 fn () =
    let input = readInput fn
    let height = Seq.length input
    let width = Seq.length (Seq.head input)
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
        |> (columnize width)
    
    rocks |> rollNorth |> Map.values |> Seq.concat |> Seq.map (getLoad height) |> Seq.sum

let part2 fn () = 0L
