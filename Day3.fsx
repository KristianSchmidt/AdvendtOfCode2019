#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllLines("./input/3.txt")

type Direction =
    | Up of int
    | Down of int 
    | Left of int
    | Right of int

let parse (s : string) =
    let arr = s.ToCharArray()
    let dir = Array.head arr
    let count = String(Array.tail arr) |> int
    match dir with
    | 'U' -> Up count
    | 'D' -> Down count
    | 'L' -> Left count
    | 'R' -> Right count

let path1 = data.[0].Split(',') |> Array.map parse
let path2 = data.[1].Split(',') |> Array.map parse

let newPos (xs,(x,y)) dir =
    match dir with
    | Up c ->
        let lst = [| for y' in y+1 .. y + c do yield (x, y') |]
        lst, Array.last lst
    | Down c ->
        let lst = [| for y' in y-1 .. -1 .. y-c do yield (x,y') |]
        lst, Array.last lst
    | Left c ->
        let lst = [| for x' in x-1 .. -1 .. x-c do yield (x',y) |]
        lst, Array.last lst
    | Right c ->
        let lst = [| for x' in x+1 .. x+c do yield (x', y) |]
        lst, Array.last lst

let pos1 =
    path1
    |> Array.scan newPos (Array.empty, (0,0))
    |> Array.collect fst

let pos2 =
    path2
    |> Array.scan newPos (Array.empty, (0,0))
    |> Array.collect fst

let manhattan (x1,y1) (x2,y2) =
    abs(x1-x2) + abs(y1-y2)

let intersections =
    Set.intersect (Set.ofArray pos1) (Set.ofArray pos2)
    |> Set.remove (0,0)
    |> Set.toArray

let ans1 =
    intersections
    |> Array.map (manhattan (0,0))
    |> Array.min

ans1

/// Part 2

let pos1' = pos1 |> Array.mapi (fun i x -> (i+1,x))
let pos2' = pos2 |> Array.mapi (fun i x -> (i+1,x))

let combSteps xy =
    let one = pos1' |> Array.find (snd >> ((=) xy)) |> fst
    let two = pos2' |> Array.find (snd >> ((=) xy)) |> fst
    one + two

let ans2 =
    intersections
    |> Array.map combSteps
    |> Array.min

ans2