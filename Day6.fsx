#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = // Is child of
    File.ReadAllLines("./input/6.txt")
    |> Array.map (fun s -> s.Split([|')'|],2))
    |> Array.map (fun [|v1;v2|] -> (v2,v1))
    |> Array.groupBy fst
    |> Array.map (fun (k,v) -> (k,Array.map snd v))
    |> Map.ofArray

let keys = data |> Map.toArray |> Array.map fst

let rec score (v) =
    let parent = Map.tryFind v data
    match parent with
    | None -> 0
    | Some vP -> 1 + score (Array.head vP)

let ans1 = keys |> Array.sumBy score

ans1

/// Part 2

let ans2 = data

let parentChain v =
    let rec f v xs =
        let parent = Map.tryFind v data
        match parent with
        | None -> xs
        | Some vP -> f (Array.head vP) ((Array.head vP) :: xs)

    f v List.empty

parentChain "YOU" |> List.iter (printfn "%s")
parentChain "SAN" |> List.iter (printfn "%s")



data |> Map.tryFind "YOU"

data |> Map.tryFind "SAN"

ans2