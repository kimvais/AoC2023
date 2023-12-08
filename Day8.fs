module AoC2023.Day8

open System
open System.Text.RegularExpressions
open AoC2023.Prelude


let rx =
    Regex(@"(?<label>[A-Z]{3}) = \((?<left>[A-Z]{3}), (?<right>[A-Z]{3})\)")

type Node = { Label: string; Left: string; Right: string }
type State = {Node: Node; Steps: int64}

let parseNode s =
    let m = rx.Match(s)

    { Label = m.Groups["label"].Value
      Left = m.Groups["left"].Value
      Right = m.Groups["right"].Value }


let rec traverse (nodes: Map<string,Node>) instructions state =
    let idx = state.Steps % (Array.length instructions |> int64) |> int
    match state.Steps % 1000L with
    | 0L -> printfn "%d..." state.Steps
    | _ -> ()
    match state.Node.Label with
    | "ZZZ" -> state.Steps
    | _ -> 
        let move = instructions.[idx]
        let nextNodeLabel =
            match move with
            | 'R' -> state.Node.Right
            | 'L' -> state.Node.Left
            | '_' -> failwith "wrong move %c" move
        let state' = {Node=nodes.[nextNodeLabel]; Steps=state.Steps + 1L }
        traverse nodes instructions state'
    
let part1 fn () =
    let input = readInputDelimByEmptyLine fn
    let instructions = input.[0] |> Array.ofSeq
    let nodes = input.[1] |> splitByLinefeed |> Seq.map (parseNode  >> fun n -> n.Label, n) |> Map.ofSeq
    traverse nodes instructions {Node=nodes.["AAA"]; Steps=0L }

let part2 fn () = 
    let input = readInputDelimByEmptyLine fn
    let instructions = input.[0] |> Array.ofSeq
    let nodes = input.[1] |> splitByLinefeed |> Seq.map (parseNode  >> fun n -> n.Label, n) |> Map.ofSeq
    nodes.Values |> Seq.filter (fun n -> n.Label.[2] = 'Z') |> printfn "%A"
    0L
