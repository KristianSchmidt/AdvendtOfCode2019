#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllText("./input/11.txt").Split(',')
           |> Array.map int64
           |> Array.mapi (fun i x -> (int64 i,int64 x))
           |> Map.ofArray

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
      Input : int64
      RelativeBase : int64
      Outputs : int64 list
      Instructions : Map<int64,int64> }

let increasePosBy i ampState = { ampState with Pos = ampState.Pos + i }

let setPosTo i ampState = { ampState with Pos = i }

let addOutput output ampState = { ampState with Outputs = output :: ampState.Outputs }

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
    | Halt -> None
    | Input inputMode ->
        let storeIdx = findStoreIdx (pos + 1L) inputMode
            
        let newState =
            state
            |> increasePosBy 2L
            |> modInstrAt storeIdx state.Input
            
        Some newState
    | Output fstMode ->
        let output = findReadIdx (pos + 1L) fstMode
        
        let newState =
            increasePosBy 2L state
            |> addOutput output
        
        Some newState
    | AdjustRelativeBase fstMode ->
        let op1 = findReadIdx (pos + 1L) fstMode
        
        let newState =
            state
            |> adjustRelativeBase op1
            |> increasePosBy 2L

        Some newState
    | EqLessThan ((fstMode, sndMode, outMode), operation) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let op3 = findStoreIdx (pos + 3L) outMode
        
        let newState =
            state
            |> increasePosBy 4L
            |> modInstrAt op3 (if (operation op1 op2) then 1L else 0L)
            
        Some newState
    | JumpIfTrueFalse ((fstMode, sndMode), test) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let newState = 
            if (test op1) then
                setPosTo op2 state
            else
                increasePosBy 3L state

        Some newState
    
    | AddMul ((fstMode, sndMode, outMode), operation) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let storeIdx = findStoreIdx (pos + 3L) outMode

        let res = operation op1 op2
        
        let newState =
            state
            |> modInstrAt storeIdx res
            |> increasePosBy 4L

        Some newState

type Direction =
    | Up
    | Down
    | Left
    | Right

let newPos (x,y) dir =
    match dir with
    | Up -> (x, y + 1)
    | Down -> (x, y - 1)
    | Left -> (x - 1, y)
    | Right -> (x + 1, y)

type Turn = | Left90 | Right90

module Turn =
    let fromInput = function | 0L -> Left90 | 1L -> Right90

let newDirection dir turn =
    match dir, turn with
    | Up, Left90 -> Left
    | Up, Right90 -> Right
    | Down, Left90 -> Right
    | Down, Right90 -> Left
    | Left, Left90 -> Down
    | Left, Right90 -> Up
    | Right, Left90 -> Up
    | Right, Right90 -> Down

type Color = | Black | White

module Color =
    let toInput = function | Black -> 0L | White -> 1L
    let fromInput = function | 0L -> Black | 1L -> White
    let getColor k m = Map.tryFind k m |> Option.defaultValue Black
    let draw = function | Black -> "█" | White -> "░"
    

let run program startColor =
    let rec f pos direction colorMap state =
        let nextStateOpt = play state
        match nextStateOpt with
        | None -> colorMap
        | Some newState when newState.Outputs.Length = 2 ->
            let (color,turn) =
                match newState.Outputs with
                // Note these outputs seem like they're in reverse, but
                // it's because they're sequentially added to a linked list.
                | [turn; col] -> (Color.fromInput col, Turn.fromInput turn)
                | _ -> failwithf "."

            let newDir = newDirection direction turn
            let pos' = newPos pos newDir
            let newColorMap = colorMap |> Map.add pos color
            let nextInput = Color.getColor pos' colorMap |> Color.toInput
            let newState' = { newState with Outputs = List.empty; Input = nextInput }
            
            f pos' newDir newColorMap newState'
        | Some newState ->
            f pos direction colorMap newState
    
    let initState = { Pos = 0L
                      Input = Color.toInput startColor
                      Outputs = List.empty
                      Instructions = program
                      RelativeBase = 0L      }
    
    f (0,0) Up Map.empty initState

let ans1 = run data Black |> Map.count

ans1

/// Part 2

let squares = run data White |> Map.toList

let (xMin,xMax, yMin, yMax) =
    let xs = squares |> List.map (fst >> fst)
    let ys = squares |> List.map (fst >> snd)
    List.min xs, List.max xs, List.min ys, List.max ys
    
let shifted =
    squares
    |> List.map (fun ((x,y),v) -> (x+abs(xMin), y+abs(yMin)),v) 

let listToString (xs : ((int*int)*Color) list) =
    let noYs = xs |> List.map (fun ((x,y),c) -> x, Color.draw c) |> Map.ofList
    [ 0 .. xMax ] |> List.map (fun i -> Map.tryFind i noYs |> Option.defaultValue " ") |> String.concat ""

let ans2 =
    shifted
    |> List.groupBy (fst >> snd)
    |> List.map (fun (k,v) -> k, listToString v)
    |> List.sortByDescending fst

ans2