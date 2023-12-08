module AoC2023.Day8

open System
open System.Text.RegularExpressions
open AoC2023.Prelude


let rx =
    Regex(@"(?<label>[A-Z1-2]{3}) = \((?<left>[A-Z1-2]{3}), (?<right>[A-Z1-2]{3})\)")

type Node = { Label: string; Left: string; Right: string }
type State = {Nodes: Node array; Steps: int64; Hits:Map<string,int64 list>}

let parseNode s =
    let m = rx.Match(s)

    { Label = m.Groups["label"].Value
      Left = m.Groups["left"].Value
      Right = m.Groups["right"].Value }

let check1 (node: Node) =
    node.Label = "ZZZ"

let check2 node =
    node.Label.[2] = 'Z'

let rec addHits found hits (round:int64) =
    match found with
    | [] -> hits
    | head :: tail ->
        let hit = head.Label[..1]
        addHits tail (hits |> Map.add hit (hits.[hit] @ [round]) ) round
        
[<TailCall>] 
let rec traverse check (nodes: Map<string,Node>) instructions state =
    let idx = state.Steps % (Array.length instructions |> int64) |> int
    match (state.Hits.Values |> Seq.forall (fun l -> ((List.isEmpty l) |> not))) with
    | true -> state.Hits
    | false -> 
        let move = instructions.[idx]
        let nextNodeLabels =
            match move with
            | 'R' -> state.Nodes |> Array.map _.Right 
            | 'L' -> state.Nodes |> Array.map _.Left
            | '_' -> failwith "wrong move %c" move
        let newNodes = nextNodeLabels |> Array.map (fun l -> nodes.[l])
        let found = newNodes |> Array.filter check |> List.ofArray
        let step = state.Steps + 1L
        let state' =
            match found with
                | [] -> {state with Nodes=newNodes; Steps=step}
                | _ ->
                    let hits' = addHits found state.Hits step
                    printfn "%A" hits'
                    {Nodes=(newNodes |> Array.filter (check >> not)); Steps=step; Hits=hits'}
        traverse check nodes instructions state'
    
let part1 fn () =
    let input = readInputDelimByEmptyLine fn
    let instructions = input.[0] |> Array.ofSeq
    let nodes = input.[1] |> splitByLinefeed |> Seq.map (parseNode  >> fun n -> n.Label, n) |> Map.ofSeq
    let hits = Map.empty.Add("AA", [])
    traverse check1 nodes instructions {Nodes=[|nodes.["AAA"]|]; Steps=0L; Hits=hits} |> printfn "%A"
    0L

let part2 fn () = 
    let input = readInputDelimByEmptyLine fn
    let instructions = input.[0] |> Array.ofSeq
    let nodes = input.[1] |> splitByLinefeed |> Seq.map (parseNode  >> fun n -> n.Label, n) |> Map.ofSeq
    let startNodes = nodes.Values |> Seq.filter (fun n -> n.Label.[2] = 'A') |> Array.ofSeq
    let hits = startNodes |> Seq.map (fun n -> n.Label[..1], List.empty<int64>) |> Map.ofSeq
    traverse check2 nodes instructions {Nodes=startNodes; Steps=0l; Hits=hits} |> printfn "%A"
    0L
