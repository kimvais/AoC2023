module AoC2023.Day2

open System.Text.RegularExpressions
open AoC2023.Prelude

let gameRx = Regex("Game (?<id>\d+): (?<data>.*)")
let pullRx = Regex(" ?(?<num>\d+) (?<color>red|green|blue)")


let parsePull pull =
    let m = pullRx.Match(pull)
    m.Groups["color"].Value, (int64 m.Groups["num"].Value)

type Game = { Id: int64; Reds: int64; Greens: int64; Blues: int64 }

let parseGame game =
    let m = gameRx.Match(game)
    let gameId = int64 m.Groups["id"].Value

    let pulls =
        m.Groups["data"].Value.Split(";")
        |> Array.map (fun s -> s.Split(","))

    let pulls' = pulls |> Array.map (Array.map parsePull >> Map.ofArray)

    let data =
        [ "red"; "green"; "blue" ]
        |> List.map (fun c ->
            c,
            pulls'
            |> Array.map (
                Map.tryFind c
                >> function
                    | Some n -> int64 n
                    | None -> 0L
            )
            |> Array.max)
        |> Map.ofList

    { Id = gameId
      Reds = data["red"]
      Greens = data["green"]
      Blues = data["blue"] }

let part1 fn () =
    let maxReds = 12L
    let maxGreens = 13L
    let maxBlues = 14L
    let games = readInput fn

    games
    |> Seq.map parseGame
    |> Seq.filter (fun g -> g.Reds <= maxReds && g.Greens <= maxGreens && g.Blues <= maxBlues)
    |> Seq.sumBy (fun g -> g.Id)

let part2 fn () =
    let games = readInput fn

    games
    |> Seq.map (parseGame >> fun g -> g.Reds * g.Greens * g.Blues)
    |> Seq.sum
