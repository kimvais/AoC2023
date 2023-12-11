module AoC2023.Day6

open System
open AoC2023.Prelude


let race (duration, distance) =
    seq { 1L .. duration - 1L }
    |> Seq.map (fun pressed -> (duration - pressed) * pressed)
    |> Seq.filter ((<) distance)
    |> Seq.length |> int64

let part1 fn () =
    let input =
        readInput fn
        |> Seq.map (
            _.Split(":")
            >> Seq.last
            >> _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
        )

    let times = Seq.head input |> Seq.map int64
    let distances = Seq.last input |> Seq.map int64
    Seq.zip times distances |> Seq.map race |> Seq.reduce (*)

let part2 fn () =
    let input =
        readInput fn
        |> Seq.map (
            (_.Split(":")
             >> Seq.last
             >> _.Split(" ", StringSplitOptions.RemoveEmptyEntries)
             >> String.concat "")
            >> int64
        )

    let time = Seq.head input
    let distance = Seq.last input
    race (time, distance)
