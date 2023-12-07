module AoC2023.Day7

open AoC2023.Prelude

let cardToRank =
    function
    | 'A' -> 14L
    | 'K' -> 13L
    | 'Q' -> 12L
    | 'J' -> 11L
    | 'T' -> 10L
    | c -> charToL c

type HandRank =
    | High
    | Pair
    | TwoPair
    | ThreeOfKind
    | FullHouse
    | FourOfKind
    | FiveOfKind

type Hand = { Cards: string; Rank: HandRank; Bid: int64 }

let rank =
    function
    | [ 1; 1; 1; 1; 1 ] -> High
    | [ 2; 1; 1; 1 ] -> Pair
    | [ 2; 2; 1 ] -> TwoPair
    | [ 3; 1; 1 ] -> ThreeOfKind
    | [ 3; 2 ] -> FullHouse
    | [ 4; 1 ] -> FourOfKind
    | [ 5 ] -> FiveOfKind
    | _ -> failwith "Invalid hand"

let makeHand (cards, bid) =
    let r =
        cards
        |> Seq.map cardToRank
        |> Seq.groupBy id
        |> Seq.map (snd >> Seq.length)
        |> Seq.sortDescending
        |> List.ofSeq
        |> rank

    { Cards = cards; Rank = r; Bid = bid }

let handsWithJokers (cards, bid) =
    let cards' =
        cards
        |> Seq.filter ((<>) 'J')
        |> Seq.groupBy id
        |> Seq.map (snd >> Seq.length)
        |> Seq.sortDescending
        |> List.ofSeq

    let numJokers = cards |> Seq.filter ((=) 'J') |> Seq.length

    match Seq.isEmpty cards' with
    | true -> { Cards = cards; Rank = FiveOfKind; Bid = bid }
    | false ->
        let biggestGroup = Seq.head cards' + numJokers
        let r = [ biggestGroup ] @ (List.tail cards') |> rank
        { Cards = cards; Rank = r; Bid = bid }

let sortHandWithJokers h =
    let cardList =
        h.Cards
        |> Seq.map (
            cardToRank
            >> function
                | 11L -> 1L
                | n -> n
        )
        |> List.ofSeq

    h.Rank, cardList

let totalWinnings hands = hands |> Seq.mapi (fun i c -> int64 (i + 1) * c.Bid) |> Seq.sum

let part1 fn () =
    let input = readInput fn

    let hands =
        input
        |> Seq.map (
            _.Split(" ")
            >> (fun [| cards; bid |] -> (cards, int64 bid))
            >> makeHand
        )

    let rankedHands =
        hands
        |> Seq.sortBy (fun h -> h.Rank, (h.Cards |> Seq.map cardToRank |> List.ofSeq))

    totalWinnings rankedHands

let part2 fn () =
    let input = readInput fn

    let hands =
        input
        |> Seq.map (_.Split(" ") >> (fun [| cards; bid |] -> (cards, int64 bid)))

    let rankedHands =
        hands |> Seq.map handsWithJokers |> Seq.sortBy sortHandWithJokers

    totalWinnings rankedHands
