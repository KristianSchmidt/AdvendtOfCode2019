#load "Helpers.fs"

#nowarn "0025"
#nowarn "0667"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllText("./input/17.txt").Split(',')
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
      RelativeBase : int64
      Instructions : Map<int64,int64>
      CurrentPos : int*int
      KnownTiles : Map<int*int, int>
      Inputs : int64 list
    }

let getNextPos (x,y) choice =
    match choice with
    | 1L -> (x,y+1)
    | 2L -> (x,y-1)
    | 3L -> (x-1,y)
    | 4L -> (x+1,y)

let getAllNeighbors pos =
    [ 1L, getNextPos pos 1L
      2L, getNextPos pos 2L
      3L, getNextPos pos 3L
      4L, getNextPos pos 4L
    ]
    
let setCurrentPos pos ampState = { ampState with CurrentPos = pos }
let setKnownTiles tiles ampState = { ampState with KnownTiles = tiles }
let setInputs inputs ampState = { ampState with Inputs = inputs }
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
        None
        //Some (Some 1, state)
    | Input inputMode ->
        let storeIdx = findStoreIdx (pos + 1L) inputMode
        
        printfn "Asked for input. Should not happen."
        let input = 0L
            
        //printfn "Input: %i Curr: %A" input state.CurrentPos

        let newState =
            state
            |> increasePosBy 2L
            |> modInstrAt storeIdx input
            
        Some (None, newState)
    | Output fstMode ->
        let output = findReadIdx (pos + 1L) fstMode

        let (x,y) = state.CurrentPos
        let newState =
            match output with
            | 10L ->
                increasePosBy 2L state
                |> setCurrentPos (0,y+1)
            | out ->
                let knownTiles = state.KnownTiles |> Map.add state.CurrentPos (int output)
                increasePosBy 2L state
                |> setKnownTiles knownTiles
                |> setCurrentPos (x+1,y)

        Some(Some state.KnownTiles, newState)
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
                 Instructions = program
                 RelativeBase = 0L
                 CurrentPos = (0,0)
                 KnownTiles = Map.empty
                 Inputs = List.empty
                 }

    Array.unfold play init
    |> Array.choose id

let tiles = runUntilHalt data |> Array.last

let getNeighbors (x,y) =
    [| (x,y+1);(x,y-1);(x+1,y);(x-1,y) |]

let allNeighborsAreScaffold pos m =
    getNeighbors pos
    |> Array.forall (fun n -> 35 = (Map.tryFind n m |> Option.defaultValue -1))

let ans1 =
    tiles
    |> Map.filter (fun k v -> v = 35 && allNeighborsAreScaffold k tiles)
    |> Map.toArray
    |> Array.sumBy (fun ((x,y),_) -> x*y)

ans1

/// Part 2

let draw (m : Map<(int*int),int>) =
    let row (arr : ((int*int)*int) array) =
        arr
        |> Array.sortBy (fst >> fst)
        |> Array.map (snd >> char >> string)
        |> String.concat ""
    
    m
    |> Map.toArray
    |> Array.groupBy (fst >> snd)
    |> Array.sortBy fst
    |> Array.map (snd >> row)
    |> String.concat "\n"
    |> prependNewline

draw tiles


(*
A: R,6,L,10,R,8,R,8
B: R,12,L,8,L,10
C: R,12,L,10,R,6,L,10

A,B,A,C,B,C,A,B,A,C

*)

let play2 state =
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
        None
        //Some (Some 1, state)
    | Input inputMode ->
        let storeIdx = findStoreIdx (pos + 1L) inputMode
        
        let (input::inputsLeft) = state.Inputs
            
        printfn "Input: %i" input

        let newState =
            state
            |> increasePosBy 2L
            |> modInstrAt storeIdx input
            |> setInputs inputsLeft
            
        Some (None, newState)
    | Output fstMode ->
        let output = findReadIdx (pos + 1L) fstMode

        let newState = increasePosBy 2L state

        Some(Some output, newState)
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

let runUntilHalt2 program inputs =
    let init = { Pos = 0L
                 Instructions = program
                 RelativeBase = 0L
                 CurrentPos = (0,0)
                 KnownTiles = Map.empty
                 Inputs = inputs
                 }

    Array.unfold play2 init
    |> Array.choose id

let newProgram = data |> Map.add 0L 2L

let inputs =
    "A,B,A,C,B,C,A,B,A,C\nR,6,L,10,R,8,R,8\nR,12,L,8,L,10\nR,12,L,10,R,6,L,10\nn\n".ToCharArray()
    |> Array.map int64
    |> List.ofArray

let ans2 =
    runUntilHalt2 newProgram inputs
    |> Array.last
