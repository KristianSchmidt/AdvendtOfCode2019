#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllText("./input/23.txt").Split(',')
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
      Inputs : int64 list
      Outputs : int64 list
      IsReceivingEmptyQueue : bool
    }

type XY = { X : int64; Y : int64 }

let setInputs inputs ampState = { ampState with Inputs = inputs }
let increasePosBy i ampState = { ampState with Pos = ampState.Pos + i }

let addOutput o ampState = { ampState with Outputs = o :: ampState.Outputs }
let clearOutput ampState = { ampState with Outputs = List.empty }

let setReceivingFalse ampState = { ampState with IsReceivingEmptyQueue = false }
let setReceivingTrue ampState = { ampState with IsReceivingEmptyQueue = true }

let addInput xy ampState =
    { ampState with Inputs = List.append ampState.Inputs [xy.X; xy.Y] }

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
    let state = state

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
        failwithf "Machine halted"
        state
    | Input inputMode ->
        let storeIdx = findStoreIdx (pos + 1L) inputMode
            
        let (input,inputsLeft) =
            match state.Inputs with
            | i :: is -> i, is
            | [] -> -1L, []

        //printfn "Input: %i" input

        let newState =
            state
            |> increasePosBy 2L
            |> modInstrAt storeIdx input
            |> setInputs inputsLeft
            |> (fun s -> if (input = -1L) then setReceivingTrue s else s)
            
        newState
    | Output fstMode ->
        let output = findReadIdx (pos + 1L) fstMode

        let newState =
            increasePosBy 2L state
            |> addOutput output

        newState
    | AdjustRelativeBase fstMode ->
        let op1 = findReadIdx (pos + 1L) fstMode
        
        let newState =
            state
            |> adjustRelativeBase op1
            |> increasePosBy 2L
            |> setReceivingFalse

        newState
    | EqLessThan ((fstMode, sndMode, outMode), operation) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let op3 = findStoreIdx (pos + 3L) outMode
        
        let newState =
            state
            |> increasePosBy 4L
            |> modInstrAt op3 (if (operation op1 op2) then 1L else 0L)
            
        newState
    | JumpIfTrueFalse ((fstMode, sndMode), test) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let newState = 
            if (test op1) then
                setPosTo op2 state
            else
                increasePosBy 3L state

        newState
    
    | AddMul ((fstMode, sndMode, outMode), operation) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let storeIdx = findStoreIdx (pos + 3L) outMode

        let res = operation op1 op2
        
        let newState =
            state
            |> modInstrAt storeIdx res
            |> increasePosBy 4L

        newState

let machines program =
    [| 0L .. 49L |]
    |> Array.map (fun i -> { Pos = 0L
                             Instructions = program
                             RelativeBase = 0L
                             Inputs = [ i ]
                             Outputs = List.empty
                             IsReceivingEmptyQueue = false
                             })


let collectOutput (m : AmpState) =
    match m.Outputs with
    | [o3;o2;o1] -> Some (o1, { X = o2; Y = o3 }), clearOutput m
    | _ -> None, m

let mapAtIdx idx f arr =
    arr
    |> Array.mapi (fun i x -> if (i = idx) then f x else x)

let addInputs (machines : AmpState array) (outs : (int64 * XY) array) =
    outs
    |> Array.fold (fun arr (i,xy) -> mapAtIdx (int i) (addInput xy) arr) machines

let solve (machines : AmpState array) =
    let rec loop ms =
        let (outputOpts, newMs) =
            Array.map (play >> collectOutput) ms
            |> Array.unzip
        let outputs = outputOpts |> Array.choose id
        let newMs' = addInputs newMs outputs
        
        match outputs |> Array.tryFind (fun (i,_) -> i = 255L) with
        | Some (_,xy) -> xy.Y
        | None -> loop newMs'

    loop machines

let ans1 = solve (machines data)

ans1

/// Part 2

let solve2 (machines : AmpState array) =
    let mutable iter = 0L
    let rec loop ms nat lastYsent =
        iter <- iter + 1L
        if (iter % 10000L = 0L) then printfn "Iter: %i" iter
        let (outputOpts, newMs) =
            Array.map (play >> collectOutput) ms
            |> Array.unzip
        let outputs = outputOpts |> Array.choose id
        let newMs' = addInputs newMs outputs

        let newNat =
            match outputs |> Array.tryFind (fun (i,_) -> i = 255L) with
            | Some (_,xy) ->
                //printfn "Received to NAT: %A" xy
                xy
            | None -> nat
        
        let isIdle = newMs' |> Array.forall (fun m -> m.IsReceivingEmptyQueue)
        let newMs'', newYsent =
            if (isIdle && newNat.X <> -1L) then
                //printfn "%i: Everyone's waiting. Sending (%i,%i) to 0" iter newNat.X newNat.Y
                let newStates = 
                    newMs'
                    |> mapAtIdx 0 (addInput newNat)
                    |> Array.map setReceivingFalse
                newStates, Some newNat.Y
            else
                newMs', None

        match lastYsent, newYsent with
        | Some lastY, Some newY when lastY = newY -> lastY
        | _, Some _ -> loop newMs'' newNat newYsent
        | _ -> loop newMs'' newNat lastYsent


    loop machines { X = -1L; Y = -1L } None


let ans2 = solve2 (machines data)

ans2