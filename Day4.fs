module AoC2023.Day4

open AoC2023.Prelude

open System.Text.RegularExpressions

type Card =
    { Id: int
      Copies: int
      WinningNumbers: int Set
      Numbers: int Set
      Matches: int }


let parseCard (c: string) =
    let [| ci; cn |] = c.Split(":")
    let [| winning; nos |] = cn.Split(" | ")
    let cardNo = ci.Split(" ") |> Seq.last

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

    { Id = int cardNo
      Copies = 1
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
    cards |> Seq.map scoreCard |> Seq.sum

let rec addCards copies idx n (cards: Map<int,Card>) =
    match n with
    | 0 -> cards
    | _ -> 
        let card = cards.[idx]
        let card' = {card with Copies = card.Copies + copies}
        let cards' = Map.add idx card' cards
        addCards copies (idx + 1) (n - 1) cards'
         
let rec scratch idx (cards: Map<int,Card>) =
    let currentCard = cards.[idx]
    match idx with
    | i when i < Map.count cards ->
        let cards' =  addCards currentCard.Copies (idx + 1) currentCard.Matches cards
        scratch (idx + 1) cards'
    | _ -> cards
    
let part2 fn () =
    let cards = readInput fn |> Seq.map parseCard
    let cardMap = cards |> Seq.map (fun c -> (c.Id,c)) |> Map.ofSeq
    scratch 1 cardMap |> Map.values |> Seq.sumBy _.Copies |> int64
