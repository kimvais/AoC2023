module AoC2023.Day1

open System.Text.RegularExpressions
open AoC2023.Prelude

let isDigit c = 0L <= c && c <= 9L

let getTheNumber s = 10L * Seq.head s + Seq.last s


let solvePart1 lines =
    lines
    |> Seq.map (Seq.map charToL >> Seq.filter isDigit >> getTheNumber)
    |> Seq.sum

let part1 fn () = readInput fn |> solvePart1

let replaceNameWithNumber =
    function
    | "zero" -> 0L
    | "one" -> 1L
    | "two" -> 2L
    | "three" -> 3L
    | "four" -> 4L
    | "five" -> 5L
    | "six" -> 6L
    | "seven" -> 7L
    | "eight" -> 8L
    | "nine" -> 9L


let numbersPatterns =
    "one|two|three|four|five|six|seven|eight|nine|zero"

let rx = Regex(numbersPatterns)
let rxr2l = Regex(numbersPatterns, RegexOptions.RightToLeft)


let getNumbersFromLine line =
    let matchToNumberAndIdx (m: Match) = m.Index, m.Value |> replaceNameWithNumber

    let numbers =
        line
        |> Seq.mapi (fun i c -> i, (charToL c))
        |> Seq.filter (snd >> isDigit)

    let namedNumbers = rx.Matches(line) |> Seq.map matchToNumberAndIdx

    let namedNumbersR2L =
        rxr2l.Matches(line) |> Seq.map matchToNumberAndIdx

    Seq.concat [ numbers; namedNumbers; namedNumbersR2L ]

let getValue numbers =
    let tens = numbers |> Seq.minBy fst |> snd
    let ones = numbers |> Seq.maxBy fst |> snd
    tens * 10L + ones

let part2 fn () =
    let lines = readInput fn

    let lines' = lines |> Seq.map getNumbersFromLine

    lines' |> Seq.map getValue |> Seq.sum
