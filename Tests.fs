module AoC2022.Tests


open FsUnit.Xunit
open Xunit

open AoC2023

[<Fact>]
let ``day 1, part 1`` () =
    Day1.part1 "test1" () |> should equal 142L
    Day1.part1 "1" () |> should equal 53080L

[<Fact>]
let ``day1, part2`` () =
    Day1.part2 "test1b" () |> should equal 281L
    Day1.part2 "1" () |> should equal 53268L