module AoC2023.Day4

open AoC2023.Prelude

open System.Text.RegularExpressions

type Card =
    { Id: string
      WinningNumbers: int Set
      Numbers: int Set
      Matches: int }

let rx =
    Regex(@"Card (?<id>\d+): (?<winningNumbers>(\d+ *)+)\| (?<numbers>(\d+ *))+")

let parseCard (c: string) =
    let [| ci; cn |] = c.Split(":")
    let [| winning; nos |] = cn.Split(" | ")

    let winningNumbers =
        winning.Trim().Split(" ")
        |> Seq.map System.Int32.TryParse
        |> Seq.filter fst
        |> Seq.map snd
        |> Set.ofSeq

    let numbers =
        nos.Trim().Split(" ")
        |> Seq.map System.Int32.TryParse
        |> Seq.filter fst
        |> Seq.map snd
        |> Set.ofSeq

    let matches =
        Set.intersect winningNumbers numbers |> Set.count

    { Id = ci
      WinningNumbers = winningNumbers
      Numbers = numbers
      Matches = matches }
    
let scoreCard card =
    match card.Matches with
    | 0 -> 0L
    | 1 -> 1L
    | n -> pown 2 (n - 1) |> int64

let part1 fn () =
    let cards = readInput fn |> Seq.map parseCard
    printfn "%A" cards
    cards |> Seq.map scoreCard |> Seq.sum

let part2 fn () = 0L
