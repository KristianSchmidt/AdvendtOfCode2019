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

// New representation, the positions with bugs

let datap2 =
    File.ReadAllLines("./input/24.txt")
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.mapi (fun y arr -> arr |> Array.mapi (fun x c -> ((x,y,0),c)))
    |> Array.collect id
    |> Array.filter (snd >> (=)'#')
    |> Array.map fst

let examplep2 =
    [| (4,0,0);(0,1,0);(3,1,0);(0,2,0);(3,2,0);(4,2,0);(2,3,0);(0,4,0) |]

let neighbors (x,y,lvl) =
    let up = 
        match x,y with
        | _,0 -> [| (2,1,lvl-1) |]
        | 2,3 -> [| (0,4,lvl+1);(1,4,lvl+1);(2,4,lvl+1);(3,4,lvl+1);(4,4,lvl+1);|]
        | _   -> [| (x,y-1,lvl) |]

    let down =
        match x,y with
        | _,4 -> [| (2,3,lvl-1) |]
        | 2,1 -> [| (0,0,lvl+1);(1,0,lvl+1);(2,0,lvl+1);(3,0,lvl+1);(4,0,lvl+1);|]
        | _   -> [| (x,y+1,lvl) |]
    
    let left =
        match x,y with
        | 0,_ -> [| (1,2,lvl-1) |]
        | 3,2 -> [| (4,0,lvl+1);(4,1,lvl+1);(4,2,lvl+1);(4,3,lvl+1);(4,4,lvl+1);|]
        | _   -> [| (x-1,y,lvl) |] 
    
    let right =
        match x,y with
        | 4,_ -> [| (3,2,lvl-1) |]
        | 1,2 -> [| (0,0,lvl+1);(0,1,lvl+1);(0,2,lvl+1);(0,3,lvl+1);(0,4,lvl+1);|]
        | _   -> [| (x+1,y,lvl) |]

    Array.collect id [| up;down;left;right |]

let progress2 (state : Set<int*int*int>) =
    let bugs = state |> Set.toArray
    let allNeighbors = bugs |> Array.collect neighbors
    let toEvaluate = Array.append bugs allNeighbors |> Array.distinct
    let me pos = Set.contains pos state
    let bugNeighbors pos =
        neighbors pos
        |> Array.filter (fun n -> Set.contains n state)
        |> Array.length

    toEvaluate
    |> Array.filter
        (fun pos -> match me pos, bugNeighbors pos with
                    | true, 1 -> true
                    | true, _ -> false
                    | false, 1
                    | false, 2 -> true
                    | _ -> false
        )
    |> Set.ofArray

let applyNtimes n init =
    [ 1 .. n ]
    |> List.fold (fun s _ -> progress2 s) init

let ans2 =
    Set.ofArray datap2
    |> applyNtimes 200
    |> Set.toArray
    |> Array.length

ans2