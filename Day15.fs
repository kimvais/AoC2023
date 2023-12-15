module AoC2023.Day15

open AoC2023.Prelude

let hashFolder state n =
    (state + n) * 17L % 256L
    
let hash (s: string) =
    s |> Seq.map (int >> int64) |> Seq.fold hashFolder 0L
    
let part1 fn () =
    let input = readInput fn |> Seq.exactlyOne |> splitS ","
    printfn "%A" input
    input |> Seq.map hash |> Seq.sum
    
let part2 s () =
    0L