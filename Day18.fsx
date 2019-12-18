#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Tile =
    | Empty
    | Wall
    | Origin
    | Key of string
    | Door of string

module Tile =
    let isNotDoorOrWall =
        function
        | Wall | Door _ -> false
        | _ -> true

    let isKey = function | Key _ -> true | _ -> false
    let getKey = function | Key k -> Some k | _ -> None


let parseTile c =
    match c with
    | '.' -> Empty
    | '#' -> Wall
    | '@' -> Origin
    | k when Char.IsLower(c) -> Key (string k)
    | k when Char.IsUpper(c) -> Door (string k)
    | _ -> failwithf "Failed to parse %s" (string c)

let readRow y (s : string) =
    s.ToCharArray()
    |> Array.mapi (fun x c -> (x,y), parseTile c)

let data =
    File.ReadAllLines("./input/18.txt")
    |> Array.mapi readRow
    |> Array.collect id
    |> Map.ofArray

let getAllNeighbors (x,y) =
    [| (x,y+1)
       (x,y-1)
       (x-1,y)
       (x+1,y) |]

//let generateGraph (m : Map<int*int,Tile>) : Map<(int*int)*(int*int),bool> =
//        
//    m
//    |> Map.toArray
//    |> Array.filter (snd >> ((<>)Wall))
//    |> Array.collect (fst >> generateEdges)
//    |> Map.ofArray
//
//let g = generateGraph data

let generateEdges graph (pos : (int*int)) =
    getAllNeighbors pos
    |> Array.filter (fun pos' -> Map.tryFind pos' graph |> Option.defaultValue Wall |> Tile.isNotDoorOrWall)

let origin = data |> Map.findKey (fun k v -> v = Origin)

#time "on"

let removeKeyAndDoor k m =
    m
    |> Map.map (fun _ v -> match v with
                           | Key k' when k' = k -> Empty
                           | Door d when d = k.ToUpper() -> Empty
                           | _ -> v)

Helpers.BFS.bfs (generateEdges data) origin
|> Array.choose (fun pos -> Map.find pos data |> Tile.getKey)

let newGraph = removeKeyAndDoor "b" data
Helpers.BFS.bfs (generateEdges newGraph) origin
|> Array.choose (fun pos -> Map.find pos newGraph |> Tile.getKey)


let ans1 = data

ans1

/// Part 2

let ans2 = data

ans2