#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

//let data = File.ReadAllLines("./input/4.txt")

let r1 = 193651
let r2 = 649729

let nums =
    [ for i in r1 .. r2 do yield i ]
    |> List.map (fun i -> i.ToString().ToCharArray() |> Array.map (string >> int) |> Array.windowed 2)

let isNotDescending (arr : int array array) =
    arr
    |> Array.exists (fun [|i1;i2|] -> i2 < i1)
    |> not

let hasTwoSame (arr : int array array) =
    arr
    |> Array.exists (fun [|i1;i2|] -> i1 = i2)

let fulfillCriteria =
    nums
    |> List.where isNotDescending
    |> List.where hasTwoSame

let ans1 =
    fulfillCriteria
    |> List.length

ans1

/// Part 2

let spliceBack (arr : int array array) =
    let re = arr |> Array.rev |> Array.head
    let t = arr |> Array.rev |> Array.tail |> Array.rev |> Array.map Array.head
    Array.concat [t; re]

let hasAPair (arr : int array) =
    arr
    |> Array.groupBy id
    |> Array.tryFind (fun (k,v) -> Array.length v = 2)
    |> Option.isSome

let ans2 =
    fulfillCriteria
    |> List.map spliceBack
    |> List.where hasAPair
    |> List.length

ans2