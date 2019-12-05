#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllText("./input/5.txt").Split(',')
    |> Array.map int
    |> Array.mapi (fun i x -> (i,x))
    |> Map.ofArray

let separate (i : int) =
    let ones = i % 10
    let tens = (i/10)%10
    let hundreds = (i/100)%10
    let thousands = (i/1000)%10
    let tenthousands = (i/10000)
    (tenthousands, thousands, hundreds, 10*tens + ones)

let findOp (pos : int) m mode =
    let opIdx = Map.find pos m
    match mode with
    | 0 -> Map.find opIdx m
    | 1 -> opIdx

let transform input (mInit : Map<int,int>) =
    let rec f m pos =
        match separate <| Map.find pos m with
        | (_, _, _, 99) -> m // Halt
        | (outMode, sndMode, fstMode, instr) when instr = 1 || instr = 2 -> // ADD or MUL
            let op = if (instr = 1) then (+) else (*)
            let opTxt = if (instr = 1) then "ADD" else "MUL"

            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            let storeIdx = Map.find (pos + 3) m
            let res = op op1 op2
            let newMap = Map.add storeIdx res m
            printfn "%s: %i %i = [%i]%i" opTxt op1 op2 storeIdx res

            f newMap (pos + 4)
        | (outMode, sndMode, fstMode, 3) -> // Input
            let storeIdx = Map.find (pos + 1) m
            let newMap = Map.add storeIdx input m
            printfn "INPUT: %i" input
            f newMap (pos + 2)
        | (outMode, sndMode, fstMode, 4) -> // Output
            let outputIdx = Map.find (pos + 1) m
            printfn "OUT: %i" (Map.find outputIdx m)
            f m (pos + 2)
        | (outMode, sndMode, fstMode, 5) -> // Jump if true
            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            if (op1 <> 0) then
                printfn "JIT: %i" op2
                f m op2
            else
                f m (pos + 3)
        | (outMode, sndMode, fstMode, 6) -> // Jump if false
            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            if (op1 = 0) then
                printfn "JIF: %i" op2
                f m op2
            else
                f m (pos + 3)
        | (outMode, sndMode, fstMode, 7) -> // Less than
            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            let op3 = findOp (pos + 3) m 1
            let newMap =
                if (op1 < op2) then
                    Map.add op3 1 m
                else
                    Map.add op3 0 m
            f newMap (pos + 4)
        | (outMode, sndMode, fstMode, 8) -> // Equal
            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            let op3 = findOp (pos + 3) m 1
            let newMap =
                if (op1 = op2) then
                    Map.add op3 1 m
                else
                    Map.add op3 0 m
            f newMap (pos + 4)

    f mInit 0

let ans1 = data |> transform 1

ans1

/// Part 2

let ans2 = data |> transform 5

ans2