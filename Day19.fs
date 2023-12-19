module AoC2023.Day19

open System.Text.RegularExpressions
open Prelude

type Category =
    | Looks
    | Musicality
    | Aerodynamics
    | Shininess

type Item =
    { Looks: int64
      Musicality: int64
      Aerodynamics: int64
      Shininess: int64 }

type Comparison =
    | Gt of Category * int64
    | Lt of Category * int64

type Destination =
    | Reject
    | Accept
    | Workflow of string

and Rule = { Test: Comparison option; Dest: Destination }

and Workflow = { Label: string; Rules: Rule list }

let itemRx =
    Regex(@"\{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)\}")

let workFlowRx = Regex(@"(?<label>[a-z]+)\{(?<rules>.*)\}")

let getCat =
    function
    | "x" -> Looks
    | "m" -> Musicality
    | "a" -> Aerodynamics
    | "s" -> Shininess

let parseRule =
    function
    | Regex @"([axms])>(\d+):R" [ c; v ] -> { Test = Some(Gt(getCat c, int64 v)); Dest = Reject }
    | Regex @"([axms])>(\d+):A" [ c; v ] -> { Test = Some(Gt(getCat c, int64 v)); Dest = Accept }
    | Regex @"([axms])<(\d+):R" [ c; v ] -> { Test = Some(Lt(getCat c, int64 v)); Dest = Reject }
    | Regex @"([axms])<(\d+):A" [ c; v ] -> { Test = Some(Lt(getCat c, int64 v)); Dest = Reject }
    | Regex @"([axms])>(\d+):([a-z]+)" [ c; v; wf ] -> { Test = Some(Gt(getCat c, int64 v)); Dest = Workflow wf }
    | Regex @"([axms])<(\d+):([a-z]+)" [ c; v; wf ] -> { Test = Some(Lt(getCat c, int64 v)); Dest = Workflow wf }
    | Regex @"([a-z]+)" [wf] -> {Test=None; Dest=Workflow wf}
    | Regex @"A" [] -> { Test = None; Dest = Accept }
    | Regex @"R" [] -> { Test = None; Dest = Accept }

let parseWorkFlow s =
    let m = workFlowRx.Match(s)
    let rules = m.Groups["rules"].Value.Split(',')
    m.Groups["label"].Value, rules |> Array.map parseRule

let gv (m: Match) (g: string) = int64 m.Groups[g].Value

let parseItem s =
    let m = itemRx.Match(s)
    let v = gv m

    { Looks = v "x"
      Musicality = v "m"
      Aerodynamics = v "a"
      Shininess = v "s" }

let part1 fn () =
    let [| flowInput; itemInput |] = readInputDelimByEmptyLine fn

    let flows =
        flowInput
        |> splitByLinefeed
        |> Array.map parseWorkFlow
        |> Map.ofArray

    let items = itemInput |> splitByLinefeed |> Array.map parseItem
    printfn "%A" flows
    printfn "%A" items
    0L

let part2 fn () = 0L
