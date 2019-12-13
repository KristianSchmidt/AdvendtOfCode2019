#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = // Is child of
    File.ReadAllLines("./input/6.txt")
    |> Array.map (fun s -> s.Split([|')'|],2))
    |> Array.map (fun [|v1;v2|] -> (v2,v1)) // Reverse so we get child -> parent relationships
    |> Array.groupBy fst
    |> Array.map (fun (k,v) -> (k,Array.map snd v |> Array.head))
    |> Map.ofArray

let keys = data |> Map.toArray |> Array.map fst

let rec score v =
    let parent = Map.tryFind v data
    match parent with
    | None -> 0
    | Some vP -> 1 + score vP // Not tail recursive, but not a problem for an input set of this size

let ans1 = keys |> Array.sumBy score

ans1

/// Part 2

let parentChain v =
    let rec f v xs =
        let parent = Map.tryFind v data
        match parent with
        | None -> xs
        | Some vP -> f vP (vP :: xs)

    f v List.empty

let p1 = parentChain "YOU"
let p2 = parentChain "SAN"

// I originally solved this without the below code
// by copying the two parent chains to excel side by side
// There I could see the last in common and manually calculate the 
// amount of orbit changes

let lastInCommon p1 p2 =
    let rec f p1 p2 inCommon =
        match p1, p2 with
        | p1' :: p1s, p2' :: p2s ->
            if (p1' <> p2') then
                inCommon
            else
                f p1s p2s p1'
        | _ -> ""
    
    f p1 p2 ""

let lic = lastInCommon p1 p2

let allAfterLic ps lic =
    ps |> List.rev |> List.takeWhile ((<>) lic) |> List.length

let ans2 = allAfterLic p1 lic + allAfterLic p2 lic

ans2