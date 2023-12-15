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
    Day3.part1 "test3" () |> should equal 4361L
    Day3.part1 "3" () |> should equal 553079L
    
[<Fact>]
let ``day3, part2`` () =
    Day3.part2 "test3" () |> should equal 467835L
    Day3.part2 "3" () |> should equal 84363105L

[<Fact>]
let ``day4, part1`` () =
    Day4.part1 "test4" () |> should equal 13L
    Day4.part1 "4" () |> should equal 24175L

[<Fact>]
let ``day4, part2`` () =
    Day4.part2 "test4" () |> should equal 30L
    Day4.part2 "4" () |> should equal 18846301L
   
[<Fact>]
let ``day5, part1`` () =
    Day5.part1 "test5" () |> should equal 35L
    Day5.part1 "5" () |> should equal 313045984L
    
[<Fact>]
let ``day5, part2`` () =
    Day5.part2 "test5" () |> should equal 46L
    // Day5.part2 "5" () |> should equal 20283860L  // Brute force is slow
    
[<Fact>]
let ``day6, part1`` () =
    Day6.part1 "test6" () |> should equal 288L
    Day6.part1 "6" () |> should equal 345015L

[<Fact>]
let ``day6, part2`` () =
    Day6.part2 "test6" () |> should equal 71503L
    Day6.part2 "6" () |> should equal 42588603L
    
[<Fact>]
let ``day7, part1`` () =
    Day7.part1 "test7" () |> should equal 6440L
    Day7.part1 "7" () |> should equal 248105065L

[<Fact>]
let ``day7, part2`` () =
    Day7.part2 "test7" () |> should equal 5905L
    Day7.part2 "7" () |> should equal 249515436L
    
[<Fact>]
let ``day8, part1`` () =
    Day8.part1 "test8a" () |> should equal 2L
    Day8.part1 "test8b" () |> should equal 6L
    Day8.part1 "8" () |> should equal 20777L

[<Fact>]
let ``day8, part2`` () =
    Day8.part2 "test8c" () |> should equal 6L
    Day8.part2 "8" () |> should equal -1L
    
[<Fact>]
let ``day9, part1`` () =
    Day9.part1 "test9" () |> should equal 114L
    Day9.part1 "9" () |> should equal 2174807968L

[<Fact>]
let ``day9, part2`` () =
    Day9.part2 "test9" () |> should equal 2L
    Day9.part2 "9" () |> should equal 1208L
    
[<Fact>]
let ``day10, part1`` () =
    Day10.part1 "test10a" () |> should equal 4L
    Day10.part1 "test10b" () |> should equal 8L
    Day10.part1 "10" () |> should equal -1L

[<Fact>]
let ``day10, part2`` () =
    Day10.part2 "test10a" () |> should equal -1L
    Day10.part2 "test10b" () |> should equal -1L
    Day10.part2 "10" () |> should equal -1L
    
[<Fact>]
let ``day11, part1`` () =
    Day11.solve 2 "test11" () |> should equal 374L
    Day11.solve 2 "11" () |> should equal 9693756L

[<Fact>]
let ``day11, part2`` () =
    Day11.solve 10 "test11" () |> should equal 1030L
    Day11.solve 100 "test11" () |> should equal 8410L
    Day11.solve 1_000_000 "11" () |> should equal 717878258016L

[<Fact>]
let ``day14 part1`` () =
    Day14.part1 "test14" () |> should equal 136L
    Day14.part1 "14" () |> should equal 108641L

[<Fact>]
let ``day15, part1`` () =
    Day15.hash "HASH" |> should equal 52L
    Day15.part1 "test15" () |> should equal 1320L