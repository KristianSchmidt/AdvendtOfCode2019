#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllText("./input/20.txt").Split(',')
    |> Array.map int
    |> Array.mapi (fun i x -> (i,x))
    |> Map.ofArray

let transform (mInit : Map<int,int>) =
    let rec f m pos =
        match Map.find pos m with
        | 99 -> m
        | 1 -> // Addition
            let op1 =
                let op1Idx = Map.find (pos + 1) m
                Map.find op1Idx m
            let op2 =
                let op2Idx = Map.find (pos + 2) m
                Map.find op2Idx m
            let storeIdx = Map.find (pos + 3) m
            let res = op1 + op2
            let newMap = Map.add storeIdx res m
            //printfn "ADD: [%i]%i * [%i]%i = [%i]%i" op1Idx op1 op2Idx op2 store res

            f newMap (pos + 4)
        | 2 -> // Multiplication
            let op1 =
                let op1Idx = Map.find (pos + 1) m
                Map.find op1Idx m
            let op2 =
                let op2Idx = Map.find (pos + 2) m
                Map.find op2Idx m
            let storeIdx = Map.find (pos + 3) m
            let res = op1 * op2
            let newMap = Map.add storeIdx res m
            //printfn "MUL: [%i]%i * [%i]%i = [%i]%i" op1Idx op1 op2Idx op2 store res

            f newMap (pos + 4)

    f mInit 0

let ans1 =
    data
    |> Map.add 1 12
    |> Map.add 2 2
    |> transform
    |> Map.find 0

ans1

/// Part 2

let f noun verb =
    data
    |> Map.add 1 noun
    |> Map.add 2 verb
    |> transform
    |> Map.find 0

let nvInputs =
    [ for n in 1 .. 100 do
        for v in 1 .. 100 do 
            yield (n,v)      ]

let ans2 =
    nvInputs
    |> Seq.find (fun (n,v) -> f n v = 19690720)
    |> (fun (n,v) -> 100 * n + v)

ans2