module AoC2023.Day11

open AoC2023.Prelude

let containsNoGalaxies cs =
    cs |> Seq.forall ((=) '.')
    
let expand rows =
    rows |> Seq.collect (fun row ->
        if containsNoGalaxies row then seq [row; row]
        else seq [row]
        )

let part1 fn () =
    let rows = readInput fn
    let rows' =  expand rows
    let columns = Seq.transpose rows'
    let columns' = expand columns
    let data = Seq.transpose columns'
    print2d data
    0L

let part2 fn () = 0L
