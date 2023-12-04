module AoC2023.Main

open AoC2023.Prelude
open AoC2023

[<EntryPoint>]
let main argv =
    let day = argv |> getProblem

    match day with
    | "1" -> Day1.part1 "1" ()
    | "1b" -> Day1.part2 "1" ()
    | "2" -> Day2.part1 "2" ()
    | "2b" -> Day2.part2 "2" ()
    | "3" -> Day3.part1 "3" ()
    | "3b" -> Day3.part2 "3" ()
    | "4" -> Day4.part1 "4" ()
    | "4b" -> Day4.part2 "4" ()
    (*
    | "5" -> Day5.part1 "5" ()
    | "5b" -> Day5.part2 "5" ()
    | "6" -> Day6.part1 "6" ()
    | "6b" -> Day6.part2 "6" ()
    | "7" -> Day7.part1 "7" ()
    | "7b" -> Day7.part2 "7" ()
    | "8" -> Day8.part1 "8" ()
    | "8b" -> Day8.part2 "8" ()
    | "9" -> Day9.part1 "9" ()
    | "9b" -> Day9.part2 "9" ()
    | "10" -> Day10.part1 "10" ()
    | "10b" -> Day10.part2 "10" ()
    | "11" -> Day11.part1 "11" ()
    | "11b" -> Day11.part2 "11" ()
    | "12" -> Day12.part1 "12" ()
    | "12b" -> Day12.part2 "12" ()
    | "13" -> Day13.part1 "13" ()
    | "13b" -> Day13.part2 "13" ()
    | "14" -> Day14.part1 "14" ()
    | "14b" -> Day14.part2 "14" ()
    | "15" -> Day15.part1 "15" ()
    | "15b" -> Day15.part2 "15" ()
    | "16" -> Day16.part1 "16" ()
    | "16b" -> Day16.part2 "16" ()
    | "17" -> Day17.part1 "17" ()
    | "17b" -> Day17.part2 "17" ()
    | "18" -> Day18.part1 "18" ()
    | "18b" -> Day18.part2 "18" ()
    | "19" -> Day19.part1 "19" ()
    | "19b" -> Day19.part2 "19" ()
    | "20" -> Day20.part1 "20" ()
    | "20b" -> Day20.part2 "20" ()
    | "21" -> Day21.part1 "21" ()
    | "21b" -> Day21.part2 "21" ()
    | "22" -> Day22.part1 "22" ()
    | "22b" -> Day22.part2 "22" ()
    | "23" -> Day23.part1 "23" ()
    | "23b" -> Day23.part2 "23" ()
    | "25" -> Day25.part1 "25" ()
    | "24" -> Day24.part1 "24" ()
    | "24b" -> Day24.part2 "24" ()
    | "25" -> Day25.part1 "25" ()
    | "25b" -> Day25.part2 "25" ()
    *)
    | "test" -> Day4.part2 "test4" ()
    |> printfn "%d"

    0
