module AoC2023.Day18

open System
open System.Text.RegularExpressions
open AoC2023.Prelude

let rx =
    Regex(@"(?<dir>[RLUD]) (?<count>\d+) \(#(?<red>[0-9a-f]{2})(?<green>[0-9a-f]{2})(?<blue>[0-9a-f]{2})")

type Direction =
    | Right
    | Left
    | Up
    | Down

type Color = { Red: int; Green: int; Blue: int }
type Instruction = { Dir: Direction; Count: int; Color: Color }

let hexToInt s = Convert.ToInt32(s, 16)

let parseInstruction inst =
    let m = rx.Match(inst)

    { Dir =
        match m.Groups["dir"].Value with
        | "R" -> Right
        | "L" -> Left
        | "U" -> Up
        | "D" -> Down
      Count = int m.Groups["count"].Value
      Color =
        { Red = hexToInt m.Groups["red"].Value
          Green = hexToInt m.Groups["green"].Value
          Blue = hexToInt m.Groups["blue"].Value } }

let change = function
    | Right -> (0,1)
    | Left -> (0,-1)
    | Up -> (-1,0)
    | Down -> (1,0)
    
let rec travel (dir: Direction) (acc: (int*int) list) (row:int,col:int) (color:Color) (count: int) =
    match count with
    | 0 -> acc
    | n ->
        let diff = change dir
        let newPos = ((row + fst diff), (col + snd diff))
        travel dir (acc @ [newPos]) newPos color (count - 1)

let dig (acc: (int*int) list) (i: Instruction) =
    let newHoles = travel i.Dir List.empty<int*int> (List.last acc) i.Color i.Count
    acc @ newHoles


let part1 fn () =
    let instructions = readInput fn |> Seq.map parseInstruction
    printfn "%A" instructions
    instructions |> Seq.fold dig (List.empty<int*int>) |> printfn "%A"
    0L

let part2 fn () = 0L
