module AoC2023.Day11

open AoC2023.Prelude

type Galaxy = { Row: int64; Col: int64 }

let containsNoGalaxies cs = cs |> Seq.forall ((=) '.')


let expand factor emptyRows emptyCols galaxies =
    galaxies |> List.map (fun g ->
        let row = emptyRows |> Seq.takeWhile (fun r -> r < g.Row) |> Seq.length |> fun n ->
            match n with
            | 0 -> g.Row
            | x -> int64 (x*factor) + g.Row - int64 x
        let col = emptyCols |> Seq.takeWhile (fun c -> c < g.Col) |> Seq.length |> fun n ->
            match n with
            | 0 -> g.Col
            | y -> int64 (y*factor) + g.Col - int64 y
        {Row=row; Col=col}
        ) 

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

let solve expandFactor fn () =
    let rows = readInput fn
    // print2d data

    let galaxies =
        rows
        |> Seq.mapi (fun r row ->
            row
            |> Seq.mapi (fun c ch ->
                match ch with
                | '#' -> Some { Row = int64 r; Col = int64 c }
                | _ -> None)
            |> Seq.choose id)
        |> Seq.concat
        |> List.ofSeq

    let emptyRows =
        rows
        |> Seq.mapi (fun i g -> i, g)
        |> Seq.filter (snd >> containsNoGalaxies)
        |> Seq.map (fst >> int64)

    let emptyColumns =
        Seq.transpose rows
        |> Seq.mapi (fun i g -> i, g)
        |> Seq.filter (snd >> containsNoGalaxies)
        |> Seq.map (fst >> int64)
   
    let galaxies' = expand expandFactor emptyRows emptyColumns galaxies 
    let galaxyPairs = getAllPairs List.empty<Galaxy * Galaxy> galaxies'
    galaxyPairs |> Seq.map getDistance |> Seq.sum 

let part1 fn () = solve 2 fn ()

let part2 fn () = solve 1000000 fn ()
