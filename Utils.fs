module AoC2023.Utils

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let readLines filePath = File.ReadLines(filePath)

let getInputFilename s = (__SOURCE_DIRECTORY__ + (sprintf "/input/%s.txt" s))

let readInput (s: string) = getInputFilename s |> readLines

let readAsTest (s: string) =
    let fn = getInputFilename s
    File.ReadAllText(fn)

let getProblem (a: seq<string>) : string = a |> Seq.head

module Seq =
    let repeatForever s =
        let c = Seq.cache s

        seq {
            while true do
                yield! c
        }

    let filteri f =
        Seq.mapi (fun i v -> (i, v))
        >> Seq.filter (fun v -> f (fst v) (snd v))
        >> Seq.map snd

    let takeWhilePlus1 predicate s =
        seq {
            yield! Seq.takeWhile predicate s
            yield! s |> Seq.skipWhile predicate |> Seq.truncate 1
        }

let split (c: char) (s: string) = s.Split c
let splitS (sep: string) (s: string) = Regex.Split(s, sep)

let splitByLinefeed (s: string) = s.Split '\n'

let splitByTwoLinefeeds = splitS "\n\n"

let readInputDelimByEmptyLine inputfile = readInput inputfile |> String.concat "\n" |> splitByTwoLinefeeds

let charToL (c: char) = int64 c - int64 '0'

let charToInt = charToL >> int

let hexToBits (value: seq<char>) =
    value
    |> Seq.map (fun n -> $"%04d{Convert.ToString(Convert.ToInt16(n.ToString(), 16), 2) |> int}")
    |> String.concat ""
    |> Seq.map (fun c -> int c - int '0')

let hexToBits2 value =
    let raw =
        Convert.ToString(Convert.ToInt64(value.ToString(), 16), 2)
        |> Seq.map charToInt

    match Seq.length raw % 8 with
    | 6 -> Seq.append [ 0; 0 ] raw
    | 0 -> raw
    | _ -> failwith "Invalid input"

let bitsToInt bits =
    let s = bits |> Seq.map string |> String.concat ""

    Convert.ToInt64(s, 2)

let boolToSymbol falseC trueC =
    function
    | false -> falseC
    | true -> trueC

let printImage boolToString (image: bool[][]) =
    image
    |> Array.iter (fun row -> (row |> Array.map boolToString |> String.concat "" |> printfn "%s"))

    printfn ""

let rec cartesian inputs =
    match inputs with
    | h :: [] -> List.fold (fun acc elem -> [ elem ] :: acc) [] h
    | h :: t ->
        List.fold (fun cacc celem -> (List.fold (fun acc elem -> (elem :: celem) :: acc) [] h) @ cacc) [] (cartesian t)
    | _ -> []

let memoize func =
    let cache = Dictionary<_, _>()

    fun key ->
        let exists, value = cache.TryGetValue key

        if exists then
            value
        else
            let value = func key
            cache.Add(key, value)
            value

let printLit () = printf "\u2588"
let printDark () = printf "\u00b7"

module AStar =
    (* Shamelessly taken from
    https://github.com/ChrisPritchard/astar-search/
    *)
    type Config<'a> =
        {
            /// <summary>
            /// A method that, given a source, will return its neighbours.
            /// </summary>
            neighbours: 'a -> seq<'a>
            /// <summary>
            /// Given two nodes that are next to each other, return the g cost between them.
            /// The g cost is the cost of moving from one to the other directly.
            /// </summary>
            gCost: 'a -> 'a -> float
            /// <summary>
            /// Given two nodes, return the f cost between them. This is a heuristic score used from a given node to the goal.
            /// Line-of-sight distance is an example of how this might be defined.
            /// </summary>
            fCost: 'a -> float
            /// <summary>
            /// The maximum number of tiles to check - used to limit overly long searches when accuracy is not paramount
            /// </summary>
            maxIterations: int option
            isTarget: 'a -> bool
        }

    let search<'a when 'a: comparison> start config : seq<'a> option =

        let rec reconstructPath cameFrom current =
            seq {
                yield current

                match Map.tryFind current cameFrom with
                | None -> ()
                | Some next -> yield! reconstructPath cameFrom next
            }

        let rec crawler closedSet (openSet, gScores, fScores, cameFrom) =
            match config.maxIterations with
            | Some c when c = Set.count closedSet -> None
            | _ ->
                match List.sortBy (fun n -> Map.find n fScores) openSet with
                | current :: _ when config.isTarget current -> Some <| reconstructPath cameFrom current
                | current :: rest ->
                    let gScore = Map.find current gScores

                    let next =
                        config.neighbours current
                        |> Seq.filter (fun n -> closedSet |> Set.contains n |> not)
                        |> Seq.fold
                            (fun (openSet, gScores, fScores, cameFrom) neighbour ->
                                let tentativeGScore = gScore + config.gCost current neighbour

                                if
                                    List.contains neighbour openSet
                                    && tentativeGScore >= Map.find neighbour gScores
                                then
                                    (openSet, gScores, fScores, cameFrom)
                                else
                                    let newOpenSet =
                                        if List.contains neighbour openSet then
                                            openSet
                                        else
                                            neighbour :: openSet

                                    let newGScores = Map.add neighbour tentativeGScore gScores

                                    let newFScores =
                                        Map.add neighbour (tentativeGScore + config.fCost neighbour) fScores

                                    let newCameFrom = Map.add neighbour current cameFrom
                                    newOpenSet, newGScores, newFScores, newCameFrom)
                            (rest, gScores, fScores, cameFrom)
                    // printfn "%A" current
                    crawler (Set.add current closedSet) next
                | _ -> None

        let gScores = Map.ofList [ start, 0. ]
        let fScores = Map.ofList [ start, config.fCost start ]
        crawler Set.empty ([ start ], gScores, fScores, Map.empty)

let inline (%!) a b = (a % b + b) % b
let inline (/%) (x: int64) = fun y -> Math.DivRem(x, y)
let inline (/%?) (x: int64) = fun y -> 
    let d, r = Math.DivRem(x, y)
    match r with
    | 0L -> d
    | _ -> failwith $"Division ended with remainder %d{r}"

let (|Regex|_|) pattern s =
    let m = Regex.Match(s, pattern)

    match m.Success with
    | false -> None
    | true -> Some(List.tail [ for g in m.Groups -> g.Value ])