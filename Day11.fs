module AoC2023.Day11

open AoC2023.Prelude

type Galaxy = { Row: int; Col: int }

let containsNoGalaxies cs = cs |> Seq.forall ((=) '.')


let expand rows =
    rows
    |> Seq.collect (fun row ->
        if containsNoGalaxies row then
            seq [ row; row ]
        else
            seq [ row ])

let rec getAllPairs (pairs: (Galaxy * Galaxy) list) (galaxies: Galaxy list) =
    match galaxies with
    | head :: tail ->
        let pairs' = pairs @ (Seq.allPairs [ head ] tail |> List.ofSeq)
        getAllPairs pairs' tail
    | _ -> pairs

let getDistance (g1, g2) =
    let rowD = Seq.max [ g1.Row; g2.Row ] - Seq.min [ g1.Row; g2.Row ]
    let colD = Seq.max [ g1.Col; g2.Col ] - Seq.min [ g1.Col; g2.Col ]
    rowD + colD

let part1 fn () =
    let rows = readInput fn
    let rows' = expand rows
    let columns = Seq.transpose rows'
    let columns' = expand columns
    let data = Seq.transpose columns'
    // print2d data

    let galaxies =
        data
        |> Seq.mapi (fun r row ->
            row
            |> Seq.mapi (fun c ch ->
                match ch with
                | '#' -> Some { Row = r; Col = c }
                | _ -> None)
            |> Seq.choose id)
        |> Seq.concat
        |> List.ofSeq

    let galaxyPairs = getAllPairs List.empty<Galaxy * Galaxy> galaxies
    galaxyPairs |> Seq.map getDistance |> Seq.sum |> int64

let part2 fn () = 0L
