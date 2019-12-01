#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllLines("./input/1.txt") |> Array.map float

let transform (d : float) = int (floor(d / 3.)) - 2

let ans1 =
    data
    |> Array.sumBy transform

ans1

/// Part 2

let transformRec (d : float) =
    let rec f totalSum d =
        let d' = transform d
        if (d' <= 0) then
            totalSum
        else
            f (totalSum + d') (float d')
    f 0 d

let ans2 =
    data
    |> Array.sumBy transformRec

ans2