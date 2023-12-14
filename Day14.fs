module AoC2023.Day14

open System.Threading
open AoC2023.Prelude

type Tile =
    | Round
    | Cube
    | Empty

type Direction =
    | North
    | South
    | East
    | West


let getShape =
    function
    | 'O' -> Round
    | '#' -> Cube
    | _ -> Empty

let getSymbol =
    function
    | Round -> "O"
    | Cube -> "#"
    | Empty -> "."

type Field = { Blocks: (int * int) array; MaxRow: int; MaxCol: int }

let printMap rocks =
    rocks
    |> Seq.iter (fun r -> String.concat "" (r |> Seq.map getSymbol) |> printfn "%s")

let getCoordsByType t rocks =
    rocks
    |> Seq.mapi (fun r xs -> xs |> Seq.mapi (fun c rock -> rock, (r, c)))
    |> Seq.concat
    |> Seq.filter (fst >> (=) t)
    |> Seq.map snd

let byRow xs =
    xs
    |> Seq.groupBy fst
    |> Seq.map (fun (r, cs) -> r, cs |> Seq.map snd |> List.ofSeq)
    |> Map.ofSeq

let byColumn xs =
    xs
    |> Seq.groupBy snd
    |> Seq.map (fun (c, rs) -> c, rs |> Seq.map fst |> List.ofSeq)
    |> Map.ofSeq

let massage map =
    map
    |> Map.toSeq
    |> Seq.map (fun (row, xs) -> xs |> Seq.map (fun (col, t) -> (row, col), t))
    |> Seq.concat
    |> Map.ofSeq

let merge m1 m2 = Map.fold (fun acc key value -> Map.add key value acc) m1 m2

let printField field rocks =
    let rocks' =
        rocks
        |> byRow
        |> Map.map (fun _ xs -> xs |> List.map (fun x -> x, Round)) |> massage

    let blocks =
        field.Blocks
        |> byRow
        |> Map.map (fun _ xs -> xs |> List.map (fun x -> x, Cube)) |> massage

    let items =
        (merge rocks' blocks)

    seq [ 0 .. field.MaxRow ]
    |> Seq.iter (fun row ->
        seq [ 0 .. field.MaxCol]
        |> Seq.iter (fun col ->
            let sym =
                match items |> Map.tryFind (row, col) with
                | Some s -> getSymbol s
                | None -> getSymbol Empty

            printf $"%s{sym}")

        printfn "")

let part1 fn () =
    let input = readInput fn
    let data = input |> Seq.map (Seq.map getShape)
    let immovables = data |> getCoordsByType Cube |> Array.ofSeq
    let rocks = data |> getCoordsByType Round

    let field =
        { Blocks = immovables
          MaxRow = Seq.length data - 1
          MaxCol = (Seq.length (Seq.head data) - 1)}

    printField field rocks
    0L

let part2 fn () = 0L
