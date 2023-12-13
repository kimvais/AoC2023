module AoC2023.Day13

open AoC2023.Prelude

let findMirrorsCandidates =
    Array.pairwise
    >> Array.mapi (fun i (a, b) ->
        match a = b with
        | true -> Some (i + 1)
        | false -> None)
    >> Array.choose id
    
// let findMirrorsInColumns = Array.transpose >> findMirrorsCandidates

let checkIfMirror (arr1,arr2) =
    let before = Array.length arr1
    let pairs = Seq.zip arr1 arr2
    match pairs |> Seq.forall (fun (a,b) -> a = b) with
    | true -> Some (int64 before)
    | false -> None
    
let tryFindMirrors arr =
    let candidates = arr |> findMirrorsCandidates
    let candidates' = candidates |> Seq.map (fun i -> arr |> Array.splitAt i |> fun (a,b)-> Array.rev a,b)
    candidates' |> Seq.map checkIfMirror |> Seq.choose id
    
let findMirrors arr =
    let mirrors = tryFindMirrors arr
    match Seq.isEmpty mirrors with
    | true -> arr |> Array.transpose |> tryFindMirrors |> Seq.exactlyOne 
    | false -> mirrors |> Seq.exactlyOne |> (*) 100L
    
let part1 fn () =
    let maps =
        readInputDelimByEmptyLine fn
        |> Array.map (splitByLinefeed >> Array.map (Seq.map id >> Array.ofSeq))
    

    maps |> Array.map findMirrors |> Seq.sum

let part2 fn () = 0L
