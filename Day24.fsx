#load "Helpers.fs"

open System
open System.IO
open System.Collections

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllLines("./input/24.txt")
    |> Array.map (fun s -> s.ToCharArray())

let data2 =
    [|
    [| '.';'.';'.';'.';'.' |]
    [| '.';'.';'.';'.';'.' |]
    [| '.';'.';'.';'.';'.' |]
    [| '#';'.';'.';'.';'.' |]
    [| '.';'#';'.';'.';'.' |] 
    |]

let intFromBitArray (ba : BitArray) =
    let arr = Array.replicate 1 0
    ba.CopyTo(arr, 0)
    arr.[0]

let toBitArray (arr : char array array) =
    Array.concat arr
    |> Array.map (function | '#' -> true | _ -> false)
    |> BitArray

let toInt (arr : char array array) = arr |> toBitArray |> intFromBitArray

let progress (arr : char array array) =
    let neighbors (x,y) =
       let coords = [| (x-1,y); (x+1,y);(x,y-1);(x,y+1)|]
                    |> Array.filter (fun (x,y) -> x >= 0 && y >= 0 && y <= 4 && x <= 4)
                    |> Array.map (fun (x,y) -> arr.[y].[x])
       let bugs = coords |> Array.filter ((=)'#') |> Array.length
       bugs
    let me (x,y) = arr.[y].[x]
    let nextField pos =
        match me pos, neighbors pos with
        | '#', 1 -> '#'
        | '#', _ -> '.'
        | '.', 1
        | '.', 2 -> '#'
        | _ -> '.'
    
    [| 0 .. 4 |]
    |> Array.map (fun y -> [| 0 .. 4 |] |> Array.map (fun x -> nextField (x,y)))

let solve init =
    let seen = Set.ofList [ toInt init ]

    let rec loop state seen =
        let nextState = progress state
        let asInt = toInt nextState
        if (Set.contains asInt seen) then
            asInt
        else
            loop nextState (Set.add asInt seen)

    loop init seen

let ans1 = solve data

ans1

/// Part 2

let ans2 = data

ans2