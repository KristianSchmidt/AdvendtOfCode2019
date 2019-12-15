#load "Helpers.fs"

open System
open System.IO

#nowarn "0025"

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllText("./input/13.txt").Split(',')
           |> Array.map int64
           |> Array.mapi (fun i x -> (int64 i,int64 x))
           |> Map.ofArray


type Tile =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball

module Tile =
    let ofInput =
        function
        | 0L -> Empty
        | 1L -> Wall
        | 2L -> Block
        | 3L -> Paddle
        | 4L -> Ball
    let draw =
        function
        | Empty  -> " "
        | Wall   -> "█"
        | Block  -> "x"
        | Paddle -> "█"
        | Ball   -> "o"

type TileOrScore = | Tile of Tile | Score of int64

let parseTileOrScore (x,y,z) =
    match (x,y) with
    | (-1L, 0L) -> Score z
    | _ -> Tile (Tile.ofInput z)


type ParameterMode =
    | Position // 0
    | Immediate // 1
    | Relative // 2

let parseParamMode i =
    match i with
    | 0L -> Position
    | 1L -> Immediate
    | 2L -> Relative 
    |_ -> failwithf "Wrong parameter mode %i" i

let separate (i : int64) =
    let ones = i % 10L
    let tens = (i/10L)%10L
    let hundreds = (i/100L)%10L
    let thousands = (i/1000L)%10L
    let tenthousands = (i/10000L)
    (parseParamMode tenthousands, parseParamMode thousands, parseParamMode hundreds, 10L*tens + ones)

type ParameterDuo = ParameterMode * ParameterMode

type ParameterTriple = ParameterMode * ParameterMode * ParameterMode

type Instr =
    | Halt
    | Add of ParameterTriple
    | Mul of ParameterTriple
    | JumpIfTrue of ParameterDuo
    | JumpIfFalse of ParameterDuo
    | LessThan of ParameterTriple
    | Equal of ParameterTriple
    | AdjustRelativeBase of ParameterMode
    | Input of ParameterMode
    | Output of ParameterMode

let (|AddMul|_|) =
    function
    | Add param -> Some (AddMul (param, (+)))
    | Mul param -> Some (AddMul (param, (*)))
    | _ -> None

let (|EqLessThan|_|) =
    function
    | Equal param    -> Some (EqLessThan (param, (=)))
    | LessThan param -> Some (EqLessThan (param, (<)))
    | _ -> None

let (|JumpIfTrueFalse|_|) =
    function
    | JumpIfTrue param  -> Some (JumpIfTrueFalse (param, (<>)0L))
    | JumpIfFalse param -> Some (JumpIfTrueFalse (param, (=)0L))
    | _ -> None

let parseInstr (i : int64) =
    let (outMode, sndMode, fstMode, instr) = separate i
    match instr with
    | 1L -> Add (fstMode, sndMode, outMode)
    | 2L -> Mul (fstMode, sndMode, outMode)
    | 3L -> Input fstMode
    | 4L -> Output fstMode
    | 5L -> JumpIfTrue (fstMode, sndMode)
    | 6L -> JumpIfFalse (fstMode, sndMode)
    | 7L -> LessThan (fstMode, sndMode, outMode)
    | 8L -> Equal (fstMode, sndMode, outMode)
    | 9L -> AdjustRelativeBase fstMode
    | 99L -> Halt

type AmpState =
    { Pos : int64
      RelativeBase : int64
      BallPositions : (int64 * int64) list
      Score : int64
      PaddlePosition : int64*int64
      CollectedOutput : int64 list
      Screen : Map<(int64*int64), Tile>
      Instructions : Map<int64,int64> }

let setScore s ampState =
    { ampState with Score = s }

let addBallPos pos ampState =
    { ampState with BallPositions = pos :: ampState.BallPositions }

let setPaddlePos pos ampState =
    { ampState with PaddlePosition = pos }

let setScreen screen ampState =
    { ampState with Screen = screen }

let increasePosBy i ampState = { ampState with Pos = ampState.Pos + i }

let setPosTo i ampState = { ampState with Pos = i }

let modInstrAt i newVal ampState =
    { ampState with Instructions = ampState.Instructions |> Map.add i newVal }

let adjustRelativeBase adj ampState =
    { ampState with RelativeBase = ampState.RelativeBase + adj }

let findNonNeg i m =
    if (i >= 0L) then Map.tryFind i m |> Option.defaultValue 0L
    else failwithf "Negative indexes not allowed: %i" i
    
let play state =
    let currentProgram = state.Instructions
    let nextInstr = Map.find state.Pos currentProgram
                    |> parseInstr
    let pos = state.Pos

    let findReadIdx pos paramMode =
        let opIdx = Map.find pos currentProgram
        match paramMode with
        | Position -> findNonNeg opIdx currentProgram
        | Immediate -> opIdx
        | Relative -> findNonNeg (opIdx + state.RelativeBase) currentProgram

    let findStoreIdx i outMode =
        match outMode with
        | Position -> Map.find i currentProgram
        | Relative -> (Map.find i currentProgram) + state.RelativeBase
        | Immediate -> failwithf "Immediate mode not allowed for store idxes"
        
    match nextInstr with
    | Halt ->
        printfn "Score: %i" state.Score
        None
    | Input inputMode ->
        let storeIdx = findStoreIdx (pos + 1L) inputMode
         
        let input =
            match state.BallPositions with
            | (x2,y2)::(x1,y1)::_ ->
                //  |> int64
                let vertical = Math.Sign(y1-y2)
                match vertical with
                | -1 ->
                    // try to project where it will be at 21
                    
                    let direction = Math.Sign(x2-x1) |> int64
                    let distFrom21 = 21L - y2
                    let projection = x2 + direction * distFrom21
                    let choice = Math.Sign(projection - (fst state.PaddlePosition)) |> int64
                    printfn "(%i,%i) -> (%i,%i)" x1 y1 x2 y2
                    printfn "Direction: %i. DistFrom21: %i" direction distFrom21
                    printfn "Paddle: (%i,%i). Projection: (%i,21)" (fst state.PaddlePosition) (snd state.PaddlePosition) projection
                    printfn "Choice: %i" choice
                    printfn ""
                    choice
                | 1 ->
                    let choice = int64 (Math.Sign(x2 - fst state.PaddlePosition))
                    printfn "(%i,%i), Paddle: (%i,%i)" x2 y2 (fst state.PaddlePosition) (snd state.PaddlePosition)
                    printfn "Upwards choice: %i" choice
                    printfn ""
                    choice
                | 0 -> 0L// stagnant
            | _ :: _ ->
                0L
            | _ ->
                failwithf "Have not received any ball position"

        //let input = 0L

        let newState =
            state
            |> increasePosBy 2L
            |> modInstrAt storeIdx input
            
        Some (None, newState)
    | Output fstMode ->
        let output = findReadIdx (pos + 1L) fstMode
        
        let newState = increasePosBy 2L state

        let parsed, newCollected =
            match newState.CollectedOutput with
            | _ :: _ :: _ :: [] -> failwithf "Should not happen"
            | y :: x :: [] ->
                Some (x,y, parseTileOrScore (x,y,output)), List.empty
            | _ -> None, output :: newState.CollectedOutput

        let newState', newScreen =
            match parsed with
            | Some (_,_,Score score) ->
                setScore score newState, None // set score
            | Some (x,y, Tile t) ->
                let f = match t with
                        | Ball -> addBallPos (x,y)
                        | Paddle -> setPaddlePos (x,y)
                        | _ -> id
                f newState, Some (Map.add (x,y) t newState.Screen)
            | _ -> newState, None

        let newState'', screenOut =
            let n = { newState' with CollectedOutput = newCollected }
            match newScreen with
            | Some ns ->
                n |> setScreen ns,
                if (ns.Count >= 888) then Some ns else None
            | None -> n, None

        Some (screenOut, newState'')
    | AdjustRelativeBase fstMode ->
        let op1 = findReadIdx (pos + 1L) fstMode
        
        let newState =
            state
            |> adjustRelativeBase op1
            |> increasePosBy 2L

        Some (None, newState)
    | EqLessThan ((fstMode, sndMode, outMode), operation) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let op3 = findStoreIdx (pos + 3L) outMode
        
        let newState =
            state
            |> increasePosBy 4L
            |> modInstrAt op3 (if (operation op1 op2) then 1L else 0L)
            
        Some (None, newState)
    | JumpIfTrueFalse ((fstMode, sndMode), test) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let newState = 
            if (test op1) then
                setPosTo op2 state
            else
                increasePosBy 3L state

        Some (None, newState)
    
    | AddMul ((fstMode, sndMode, outMode), operation) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let storeIdx = findStoreIdx (pos + 3L) outMode

        let res = operation op1 op2
        
        let newState =
            state
            |> modInstrAt storeIdx res
            |> increasePosBy 4L

        Some (None, newState)

let runUntilHalt program =
    let init = { Pos = 0L
                 BallPositions = []
                 Score = -1L
                 PaddlePosition = (-1L,-1L)
                 Instructions = program
                 CollectedOutput = List.empty
                 Screen = Map.empty
                 RelativeBase = 0L      }

    Array.unfold play init
    |> Array.choose id

//let everyNth n seq = 
//    seq |> Seq.mapi (fun i el -> el, i)              // Add index to element
//        |> Seq.filter (fun (el, i) -> i % n = n - 1) // Take every nth element
//        |> Seq.map fst 
//
//let ans1 =
//    runUntilHalt data 0L
//    |> everyNth 3
//    |> Seq.filter ((=)2L)
//    |> Seq.length
    
//ans1

/// Part 2

let draw (arr : ((int64*int64)*Tile) array) =
    let drawRow a =
        snd a
        |> Array.sortBy (fun ((x,_),_) -> x)
        |> Array.map (fun (_,t) -> Tile.draw t)
        |> String.concat ""

    arr
    |> Array.groupBy (fun ((_,y),_) -> y)
    |> Array.sortBy fst
    |> Array.map drawRow
    |> String.concat "\n"
    |> prependNewline

let containsBall (m : Map<_,Tile>) =
    m |> Seq.exists (fun kv -> kv.Value = Ball)

let freePlayProgram =
    runUntilHalt (data |> Map.add 0L 2L)
    |> Array.filter containsBall

freePlayProgram.Length

let drawi i = draw <| (Map.toArray freePlayProgram.[i])

drawi 0
drawi 1
drawi 2
drawi 3
drawi 4
drawi 5
drawi 6
drawi 7
drawi 8
drawi 9
drawi 10
drawi 11
drawi 12
drawi 13
drawi 14
drawi 15
drawi 16
drawi 17
drawi 18
drawi 19
drawi 20
drawi 21
drawi 22
drawi 23
drawi 24
drawi 25
drawi 26



let ans2 = data

ans2