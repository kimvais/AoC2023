module AoC2023.Day9

open AoC2023.Prelude

let rec solve rows =
    match List.last rows |> List.forall ((=) 0L) with
    | true -> rows
    | false ->
        let row =
            List.last rows |> List.pairwise |> List.map (fun (a, b) -> b - a)

        let rows' = rows @ [ row ]
        solve rows'


let getInput fn =
    let input = readInput fn
    input |> Seq.map (split ' ' >> Seq.map int64 >> List.ofSeq)

let part1 fn () =
    let rows = getInput fn

    rows
    |> Seq.map (fun r -> solve [ r ])
    |> Seq.map (List.map List.last >> List.rev >> List.reduce (+))
    |> Seq.sum

let sub a b = b - a

let part2 fn () =
    let rows = getInput fn

    rows
    |> Seq.map (fun r -> solve [ r ])
    |> Seq.map (List.map List.head >> List.rev >> List.reduce sub)
    |> Seq.sum