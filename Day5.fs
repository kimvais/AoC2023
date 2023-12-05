module AoC2023.Day5

open System
open AoC2023.Prelude

type Map' = {
    Start: int64
    End: int64
    Offset: int64
}

let parseRow (row:string) =
    let nos = row.Split(" ", StringSplitOptions.RemoveEmptyEntries)
    let start = int64 nos.[1]
    let end' = start + (int64 nos.[2]) - 1L
    let offset = (int64 nos.[0]) - start
    {
        Start=start
        End=end'
        Offset=offset
    }

let parseMap data =
    data |> Seq.skip 1 |> Seq.map parseRow

let isMapped n m =
    n >= m.Start && n <= m.End
    
let applyMaps maps num =
    match maps |> List.filter (isMapped num) with
    | [] -> num
    | [m] -> num + m.Offset
    
let rec mapSeed stages num =
    match stages with
    | [] -> num
    | head::tail ->
        let num' = applyMaps head num
        mapSeed tail num'
        
let solve seeds maps =
    seeds |> Seq.map (mapSeed maps)  |> Seq.min

let parseInput fn = 
    let data = readInputDelimByEmptyLine fn |> Array.map splitByLinefeed
    let maps = data |> Seq.skip 1 |> Seq.map (parseMap >> List.ofSeq) |> List.ofSeq
    let seeds = data.[0][0] |> _.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Seq.skip 1 |> Seq.map int64
    seeds, maps
    
let part1 fn () =
    let seeds, maps = parseInput fn
    solve seeds maps 
    
let part2 fn () = 
    let seeds, maps = parseInput fn
    seeds |> Seq.pairwise |> printfn "%A"
    solve seeds maps 
