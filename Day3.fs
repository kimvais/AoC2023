﻿module AoC2023.Day3

open System
open System.Text.RegularExpressions
open AoC2023.Prelude

let rx = Regex(@"\d+")

type Number = { Y: int; X: int; Len: int; V: int64 }

let parseLine lineNo line =
    let matches = rx.Matches(line)

    matches
    |> Seq.map (fun m -> { Y = lineNo; X = m.Index; Len = m.Length; V = int64 m.Value })

let parseNumbers lines = lines |> Array.mapi parseLine |> Seq.concat

let isSymbol c =
    Char.IsDigit c |> not && c <> '.'
    
let getNeighbors (lines: string array) number =
    let neighbors = seq {
        let x1 = Seq.max ([ number.X - 1; 0 ])

        let x2 =
            Seq.min ([ number.X + number.Len; String.length lines.[number.Y] - 1 ])

        let y1 = Seq.max ([ number.Y - 1; 0 ])

        let y2 =
            Seq.min ([ number.Y + 1; Array.length lines - 1 ])
        if y1 < number.Y then
            let rowAbove = lines.[y1][x1..x2]
            yield! rowAbove
        if x1 > 0 then
            let charBefore = lines.[number.Y][x1]
            yield charBefore
        if x2 < String.length lines.[number.Y] - 1 then
            let charAfter = lines.[number.Y][x2]
            yield charAfter
        if y2 > number.Y then
            let rowBelow = lines.[y2][x1..x2]
            yield! rowBelow
    }
    neighbors

let part1 fn () =
    let lines = readInput fn |> Array.ofSeq

    let numbers =
        lines
        |> parseNumbers
    numbers |> Seq.iter (getNeighbors lines >> ignore)
    numbers
        |> Seq.filter (getNeighbors lines >> Seq.exists isSymbol)
        |> Seq.sumBy (fun n -> n.V)


let part2 fn () = 0L
