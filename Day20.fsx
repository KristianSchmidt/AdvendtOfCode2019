#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllLines("./input/20.txt")
    |> Array.map (fun s -> s.ToCharArray())

type Tile =
    | Empty
    | Wall
    | Space
    | InnerPortal of string
    | OuterPortal of string

module Tile =
    let isNotWallOrSpace = function | Wall | Space -> false | _ -> true

let isOuterTopFrontier (x,y) = y = 2
let isOuterBottomFrontier (x,y) = y = 116
let isOuterLeftFrontier (x,y) = x = 2
let isOuterRightFrontier (x,y) = x = 114

let isInnerTopFrontier (x,y) = y = 30 && x >= 31 && x <= 85
let isInnerBottomFrontier (x,y) = y = 88 && x >= 31 && x <= 85
let isInnerLeftFrontier (x,y) = x = 30 && y >= 31 && y <= 87
let isInnerRightFrontier (x,y) = x = 86 && y >= 31 && y <= 87

let nameAbove (x,y) (data : char array array) =
    String([| data.[y-2].[x]; data.[y-1].[x] |])

let nameBelow (x,y) (data : char array array) =
    String([| data.[y+1].[x]; data.[y+2].[x] |])

let nameLeft (x,y) (data : char array array) =
    String([| data.[y].[x-2]; data.[y].[x-1] |])

let nameRight (x,y) (data : char array array) =
    String([| data.[y].[x+1]; data.[y].[x+2] |])

let parseTile (data : char array array) pos =
    function
    | '.' ->
        if (isOuterTopFrontier pos) then
            OuterPortal (nameAbove pos data)
        else if (isOuterBottomFrontier pos) then
            OuterPortal (nameBelow pos data)
        else if (isOuterLeftFrontier pos) then
            OuterPortal (nameLeft pos data)
        else if (isOuterRightFrontier pos) then
            OuterPortal (nameRight pos data)
        else if (isInnerTopFrontier pos) then
            InnerPortal (nameBelow pos data)
        else if (isInnerBottomFrontier pos) then
            InnerPortal (nameAbove pos data)
        else if (isInnerRightFrontier pos) then
            InnerPortal (nameLeft pos data)
        else if (isInnerLeftFrontier pos) then
            InnerPortal (nameRight pos data)
        else
            Empty
    | '#' -> Wall
    | _ -> Space

let parsed (arr : char array array) =
    arr
    |> Array.mapi (fun y arr' -> arr' |> Array.mapi (fun x c -> (x,y), parseTile arr (x,y) c))
    |> Array.collect id
    |> Array.filter (snd >> (<>)Space)

let graph = parsed data
            |> Map.ofArray

let getAllNeighbors (x,y) =
    [| (x,y+1)
       (x,y-1)
       (x-1,y)
       (x+1,y) |]

let adjf graph pos =
    let t = Map.find pos graph
    let stdNeighbors =
        getAllNeighbors pos
        |> Array.map (fun n -> n,Map.tryFind n graph |> Option.defaultValue Wall)
        |> Array.filter (snd >> Tile.isNotWallOrSpace)
        |> Array.map fst
    match t with
    | InnerPortal p ->
        match Map.tryFindKey (fun k v -> v = OuterPortal p) graph with
        | Some portalPos -> Array.append [| portalPos |] stdNeighbors
        | None -> stdNeighbors
    | OuterPortal p ->
        match Map.tryFindKey (fun k v -> v = InnerPortal p) graph with
        | Some portalPos -> Array.append [| portalPos |] stdNeighbors
        | None -> stdNeighbors
    | _ -> stdNeighbors

let aa = Map.findKey (fun k v -> v = OuterPortal "AA") graph
let zz = Map.findKey (fun k v -> v = OuterPortal "ZZ") graph

let ans1 =
    BFS.bfs (adjf graph) aa
    |> Map.find zz

ans1

/// Part 2

let adjf2 (graph : Map<int*int,Tile>) ((pos,level) : (int*int)*int) =
    let t = Map.find pos graph
    let stdNeighbors =
        getAllNeighbors pos
        |> Array.map (fun n -> n,Map.tryFind n graph |> Option.defaultValue Wall)
        |> Array.filter (snd >> Tile.isNotWallOrSpace)
        |> Array.map (fst >> (fun n -> n,level))
    
    match level, t with
    | _, OuterPortal "AA"
    | _, OuterPortal "ZZ"
    | 0, OuterPortal _ -> stdNeighbors
    | l, OuterPortal p ->
        match Map.tryFindKey (fun k v -> v = InnerPortal p) graph with
        | Some portalPos -> Array.append [| (portalPos, l-1) |] stdNeighbors
        | None -> stdNeighbors
    | l, InnerPortal p ->
        match Map.tryFindKey (fun k v -> v = OuterPortal p) graph with
        | Some portalPos -> Array.append [| (portalPos, l+1) |] stdNeighbors
        | None -> stdNeighbors
    | _ -> stdNeighbors
        
let ans2 =
    BFS.bfsWithStop (adjf2 graph) (aa,0) (zz,0)
    |> Map.find (zz,0)

ans2