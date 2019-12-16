#load "Helpers.fs"

#time "on"

open System
open System.IO
open System.Linq

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllText("./input/16.txt")
    |> (fun s -> s.Trim().ToCharArray() |> Array.map (string >> int))

/// Use i as 1 indexed
let pattern n i =
    let i' = (i % (4*n))
    if (i' < n) then
        0
    else if (i' < 2*n) then
        1
    else if (i' < 3*n) then
        0
    else
        -1

let applyFFT (input : int array) = 
    let applyForN n =
        [| 1 .. input.Length |]
        |> Array.sumBy (fun i -> (pattern n i)*input.[i-1])
        |> (fun i -> (abs i) % 10)

    [| 1 .. input.Length |]
    |> Array.map applyForN

let ans1 =
    [| 1 .. 100 |]
    |> Array.fold (fun s _ -> applyFFT s) data
    |> Array.take 8
    |> Array.map string
    |> String.concat ""

ans1

/// Part 2

let data2 =
    Array.replicate 10000 data
    |> Array.concat

let readFromIdx =
    data.[0 .. 6]
    |> Array.map string
    |> String.concat ""
    |> int

let accMod10 i acc = (acc + i) % 10 

// Takes list in reverse as input  
let partialFFT (arr : int list) =
    List.scan accMod10 arr.Head arr.Tail

let replicated =
    List.ofArray data2.[readFromIdx .. ]
    |> List.rev

[ 1 .. 100 ]
|> List.fold (fun s _ -> partialFFT s) replicated
|> List.rev // Need to re-reverse the list
|> List.take 8
|> List.map string
|> String.concat ""
