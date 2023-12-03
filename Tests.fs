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

[<Fact>]
let ``day2, part1`` () =
    Day2.part1 "test2" () |> should equal 8L
    Day2.part1 "2" () |> should equal 3099L
    
[<Fact>]
let ``day2, part2`` () =
    Day2.part2 "test2" () |> should equal 2286L
    Day2.part2 "2" () |> should equal 72970L

[<Fact>]
let ``day3, part1`` () =
    Day3.part1 "test3" () |> should equal 8L
    Day3.part1 "3" () |> should equal 3099L
    
[<Fact>]
let ``day3, part2`` () =
    Day3.part2 "test3" () |> should equal 2286L
    Day3.part2 "3" () |> should equal 72970L

