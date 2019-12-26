#load "Helpers.fs"

open System
open System.IO
open System.Collections

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllLines("./input/22.txt")

type Deal =
    | NewStack
    | Cut of int
    | WithIncrement of int

let parseDeal (s : string) =
    if (s = "deal into new stack") then NewStack
    else
        let n = split " " s |> Array.last |> int
        if (s.StartsWith("deal with increment")) then
            WithIncrement n
        else if (s.StartsWith("cut")) then
            Cut n
        else
            failwithf "%s" s

let newStack arr = Array.rev arr

let cut n arr =
    let len = Array.length arr
    let cutPoint = (len + n) % len
    let (first,last) = Array.splitAt cutPoint arr
    Array.append last first
    
let increment n arr =
    let len = Array.length arr
    arr
    |> Array.permute
        (fun i ->
            //printfn "i: %i. n: %i. len: %i" i n len
            (i * n) % len)

let applyDeal arr deal =
    match deal with
    | NewStack -> newStack arr
    | Cut n -> cut n arr
    | WithIncrement n -> increment n arr

let deals =
    data
    |> Array.map parseDeal

let ans1 =
    let startDeck = [| 0 .. 10006 |]
    deals
    |> Array.fold applyDeal startDeck
    |> Array.findIndex ((=)2019)

ans1

/// Part 2

let invModMem =
    memoize (fun (n,len) -> inverseMod n len)

type Coef = { Add : bigint; Mul : bigint }

let coefMod len coef = { Add = coef.Add % len; Mul = coef.Mul % len }

let coefSquare len coef =
    // a + m (a + m i) = a + m a + m^2 i
    let add = coef.Add + (coef.Add * coef.Mul)
    let mul = coef.Mul * coef.Mul
    coefMod len { Add = add; Mul = mul }

let coefToIdx len i coef = (coef.Add + i * coef.Mul) % len

let coefNewStack (len : bigint) coef =
    { coef with Add = (-1I * coef.Add) + len - 1I; Mul = coef.Mul * -1I }

let coefIncrement len n coef =
    let multiplier = invModMem (n,len)
    if (multiplier = 0I) then printfn "Multiplier zero! (%A, %A) " n len 
    { coef with Add = coef.Add *  multiplier; Mul = coef.Mul * multiplier }

let coefCut len n coef =
    { coef with Add = coef.Add + ((len + n) % len) }

let applyInverseCoef len coef =
    function
    | NewStack -> coefNewStack len coef
    | WithIncrement n -> coefIncrement len (bigint n) coef
    | Cut n -> coefCut len (bigint n) coef

let coefInverseDeals deals len coef =
    Array.foldBack
        (fun d c -> applyInverseCoef len c d) deals coef
    |> coefMod len

let coefInverseN deals len n coef =
    [ 1 .. n ]
    |> List.fold (fun s _  -> coefInverseDeals deals len s) coef

let getMultArr (i : bigint) =
    let ba = BitArray(i.ToByteArray())
    let arr =
        [| 0 .. ba.Length - 1 |]
        |> Array.map (fun i -> ba.Item i)
        |> Array.rev
    let idx = arr |> Array.findIndex ((=)true)
    Array.skip (idx+1) arr

let coefInverseNfast deals len n coef =
    let multArr = getMultArr n
    multArr
    |> Array.fold
        (fun s t -> match t with
                    | true -> coefSquare len s |> coefInverseDeals deals len
                    | false -> coefSquare len s) coef

let len = 119315717514047I
let shuffles = 101741582076661I

let c = coefInverseDeals deals len { Add = 0I; Mul = 1I }

let coef = coefInverseNfast deals len shuffles c

let ans2 = coefToIdx len 2020I coef

ans2