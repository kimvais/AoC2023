module AoC2023.Day15

open AoC2023.Prelude

let hashFolder state n = (state + n) * 17L % 256L

let hash (s: string) = s |> Seq.map (int >> int64) |> Seq.fold hashFolder 0L

let part1 fn () =
    let input = readInput fn |> Seq.exactlyOne |> splitS ","
    input |> Seq.map hash |> Seq.sum

type Operation =
    | Assign of label: string * value: int64
    | Remove of label: string

type Lens = { Label: string; Refraction: int64 }

let parseOp op =
    match op with
    | Regex @"([a-z]+)=([0-9]+)" [ label; value ] -> Assign(label, (int64 value))
    | Regex @"([a-z]+)-" [ label ] -> Remove label
    | _ -> failwith $"Invalid operation %s{op}"

let getLabel =
    function
    | Assign(l, _) -> l
    | Remove l -> l

let arrange (boxes: Lens list array) op =
    let label = getLabel op
    let boxId = hash label |> int
    let lenses = boxes[boxId]

    let lenses' =
        match op with
        | Assign(_, refValue) ->
            let lens' = { Label = label; Refraction = refValue }

            match
                boxes[boxId]
                |> List.tryFindIndex (fun lens -> lens.Label = label)
            with
            | Some n -> List.updateAt n lens' lenses
            | None -> lenses @ [ lens' ]
        | Remove l -> lenses |> List.filter (fun lens -> lens.Label <> l)

    Array.set boxes boxId lenses'
    boxes


let part2 fn () =
    let input = readInput fn |> Seq.exactlyOne |> splitS ","
    let operations = input |> Seq.map parseOp
    let boxes = Array.init 256 (fun _ -> List.empty<Lens>)

    let boxesWithLenses =
        operations
        |> (Seq.fold arrange boxes)
        |> Seq.indexed
        |> Seq.filter (snd >> List.isEmpty >> not)

    boxesWithLenses
    |> Seq.map (fun (i, lenses) ->
        lenses
        |> List.mapi (fun j lens -> seq [ int64 i + 1L; int64 j + 1L; lens.Refraction ]))
    |> Seq.concat
    |> Seq.map (Seq.reduce (*))
    |> Seq.sum
