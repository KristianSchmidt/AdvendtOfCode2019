#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllText("./input/7.txt").Split(',')
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

let transform (phaseSettings : int list) (mInit : Map<int,int>) =
    let rec f m pos
        (isInputPhaseSetting : bool)
        (lastOutput : int)
        (phaseSetting : int) =
        match separate <| Map.find pos m with
        | (_, _, _, 99) -> lastOutput // Halt
        | (outMode, sndMode, fstMode, instr) when instr = 1 || instr = 2 -> // ADD or MUL
            let op = if (instr = 1) then (+) else (*)
            let opTxt = if (instr = 1) then "ADD" else "MUL"

            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            let storeIdx = Map.find (pos + 3) m
            let res = op op1 op2
            let newMap = Map.add storeIdx res m
            //printfn "%s: %i %i = [%i]%i" opTxt op1 op2 storeIdx res

            f newMap (pos + 4) isInputPhaseSetting lastOutput phaseSetting
        | (outMode, sndMode, fstMode, 3) -> // Input
            let storeIdx = Map.find (pos + 1) m
            let (input, isPhase) =
                if (isInputPhaseSetting) then
                    //printfn "PHASE: %i" phaseSetting
                    phaseSetting, false
                else
                    //printfn "INPUT: %i" lastOutput
                    lastOutput, true
                    
            let newMap = Map.add storeIdx input m
            f newMap (pos + 2) isPhase lastOutput phaseSetting
        | (outMode, sndMode, fstMode, 4) -> // Output
            let outputIdx = Map.find (pos + 1) m
            let output = Map.find outputIdx m
            //printfn "OUT: %i" output
            f m (pos + 2) isInputPhaseSetting output phaseSetting
        | (outMode, sndMode, fstMode, 5) -> // Jump if true
            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            if (op1 <> 0) then
                //printfn "JIT: %i" op2
                f m op2 isInputPhaseSetting lastOutput phaseSetting
            else
                f m (pos + 3) isInputPhaseSetting lastOutput phaseSetting
        | (outMode, sndMode, fstMode, 6) -> // Jump if false
            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            if (op1 = 0) then
                //printfn "JIF: %i" op2
                f m op2 isInputPhaseSetting lastOutput phaseSetting
            else
                f m (pos + 3) isInputPhaseSetting lastOutput phaseSetting
        | (outMode, sndMode, fstMode, 7) -> // Less than
            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            let op3 = findOp (pos + 3) m 1
            let newMap =
                if (op1 < op2) then
                    Map.add op3 1 m
                else
                    Map.add op3 0 m
            f newMap (pos + 4) isInputPhaseSetting lastOutput phaseSetting
        | (outMode, sndMode, fstMode, 8) -> // Equal
            let op1 = findOp (pos + 1) m fstMode
            let op2 = findOp (pos + 2) m sndMode
            let op3 = findOp (pos + 3) m 1
            let newMap =
                if (op1 = op2) then
                    Map.add op3 1 m
                else
                    Map.add op3 0 m
            f newMap (pos + 4) isInputPhaseSetting lastOutput phaseSetting

    //let outputA = f mInit 0 true 0 phaseSettings.Head
    //printfn "OUT A: %i" outputA

    phaseSettings
    |> List.fold (f mInit 0 true) 0


let ans1 = 
    Helpers.permutations [0;1;2;3;4]
    |> Seq.map (fun phase -> transform phase data)
    |> Seq.max

ans1

/// Part 2

type ParameterMode =
    | Position
    | Immediate

let parseParamMode i =
    if (i = 0) then Position else Immediate

let separate2 (i : int) =
    let ones = i % 10
    let tens = (i/10)%10
    let hundreds = (i/100)%10
    let thousands = (i/1000)%10
    let tenthousands = (i/10000)
    (tenthousands, parseParamMode thousands, parseParamMode hundreds, 10*tens + ones)

type ParameterModes = ParameterMode * ParameterMode

type Instr =
    | Halt
    | Add of ParameterModes
    | Mul of ParameterModes
    | JumpIfTrue of ParameterModes
    | JumpIfFalse of ParameterModes
    | LessThan of ParameterModes
    | Equal of ParameterModes
    | Input
    | Output

let (|AddMul|_|) =
    function
    | Add (fstMode, sndMode) -> Some (AddMul (fstMode, sndMode, (+)))
    | Mul (fstMode, sndMode) -> Some (AddMul (fstMode, sndMode, (*)))
    | _ -> None

let (|EqLessThan|_|) =
    function
    | Equal (fstMode, sndMode) -> Some (EqLessThan (fstMode, sndMode, (=)))
    | LessThan (fstMode, sndMode) -> Some (EqLessThan (fstMode, sndMode, (fun op1 op2 -> op1 < op2)))
    | _ -> None

let (|JumpIfTrueFalse|_|) =
    function
    | JumpIfTrue (fstMode, sndMode) -> Some (JumpIfTrueFalse (fstMode, sndMode, (=)0))
    | JumpIfFalse (fstMode, sndMode) -> Some (JumpIfTrueFalse (fstMode, sndMode, (<>)0))
    | _ -> None

let parseInstr (i : int) =
    let (outMode, sndMode, fstMode, instr) = separate2 i
    match instr with
    | 1 -> Add (fstMode, sndMode)
    | 2 -> Mul (fstMode, sndMode)
    | 3 -> Input
    | 4 -> Output
    | 5 -> JumpIfTrue (fstMode, sndMode)
    | 6 -> JumpIfFalse (fstMode, sndMode)
    | 7 -> LessThan (fstMode, sndMode)
    | 8 -> Equal (fstMode, sndMode)
    | 99 -> Halt

type Amp = | A | B | C | D | E
let connections =
    [ (A,B); (B,C); (C,D); (D,E); (E,A) ] |> Map.ofList

type AmpState =
    { Pos : int
      NextInput : int option
      PhaseSetting : int
      HasReceivedPhaseSetting : bool
      Instructions : Map<int,int>
    }

let increasePosBy i ampState = { ampState with Pos = ampState.Pos + i }

let setPosTo i ampState = { ampState with Pos = i }

let setNextInput i ampState = { ampState with NextInput = Some i }

let hasReceivedPhaseSetting ampState = { ampState with HasReceivedPhaseSetting = true }

let modInstrAt i newVal ampState =
    { ampState with Instructions = ampState.Instructions |> Map.add i newVal }

type OverallState =
    { AmpStates : Map<Amp,AmpState>
      CurrentlyProcessing : Amp
    }

let changeAmpState amp newState state =
    { state with AmpStates = state.AmpStates |> Map.add amp newState }

let changeCurrentlyProcessing amp state =
    { state with CurrentlyProcessing = amp }

let findOp2 pos currentProgram paramMode =
    let opIdx = Map.find pos currentProgram
    match paramMode with
    | Position -> Map.find opIdx currentProgram
    | Immediate -> opIdx

let transform2 program (phaseSettings : int list) =
    let phaseSettings = List.zip [A;B;C;D;E] phaseSettings |> Map.ofList
    
    let initStates =
        [ (A, { Pos = 0; NextInput = Some 0; HasReceivedPhaseSetting = false; PhaseSetting = Map.find A phaseSettings; Instructions = program })
          (B, { Pos = 0; NextInput = None;   HasReceivedPhaseSetting = false; PhaseSetting = Map.find B phaseSettings; Instructions = program })
          (C, { Pos = 0; NextInput = None;   HasReceivedPhaseSetting = false; PhaseSetting = Map.find C phaseSettings; Instructions = program })
          (D, { Pos = 0; NextInput = None;   HasReceivedPhaseSetting = false; PhaseSetting = Map.find D phaseSettings; Instructions = program })
          (E, { Pos = 0; NextInput = None;   HasReceivedPhaseSetting = false; PhaseSetting = Map.find E phaseSettings; Instructions = program })
        ] |> Map.ofList

    let init = { AmpStates = initStates; CurrentlyProcessing = A }
    
    let rec play state =
        let currentState = Map.find state.CurrentlyProcessing state.AmpStates
        let currentProgram = currentState.Instructions
        let nextInstr =
            Map.find currentState.Pos currentProgram
            |> parseInstr
        let pos = currentState.Pos    

        match nextInstr with
        | Halt -> state
        | Input ->
            let storeIdx = Map.find (pos + 1) currentProgram
            let input =
                match currentState.HasReceivedPhaseSetting with
                | true -> currentState.NextInput.Value
                | false -> currentState.PhaseSetting

            let newState =
                currentState
                |> increasePosBy 2
                |> hasReceivedPhaseSetting
                |> modInstrAt storeIdx input
                
            let newOverallState =
                state |> changeAmpState state.CurrentlyProcessing newState

            play newOverallState
        | Output ->
            let outputIdx = Map.find (pos + 1) currentProgram
            let output = Map.find outputIdx currentProgram
            let newStateThis = currentState |> increasePosBy 2
            let connection = connections |> Map.find state.CurrentlyProcessing
            
            let newConnectionState =
                state.AmpStates
                |> Map.find connection
                |> setNextInput output

            let newOverallState =
                state
                |> changeAmpState state.CurrentlyProcessing newStateThis
                |> changeAmpState connection newConnectionState
                |> changeCurrentlyProcessing connection

            play newOverallState
        | EqLessThan (fstMode, sndMode, operation) ->
            let op1 = findOp2 (pos + 1) currentProgram fstMode
            let op2 = findOp2 (pos + 2) currentProgram sndMode
            let op3 = findOp2 (pos + 3) currentProgram Immediate

            let newState =
                currentState
                |> increasePosBy 4
                |> modInstrAt op3 (if (operation op1 op2) then 1 else 0)
                
            let newOverallState =
                state |> changeAmpState state.CurrentlyProcessing newState
                
            play newOverallState
        | JumpIfTrueFalse (fstMode, sndMode, test) ->
            let op1 = findOp2 (pos + 1) currentProgram fstMode
            let op2 = findOp2 (pos + 2) currentProgram sndMode
            let newState = 
                if (test op1) then
                    setPosTo op2 currentState
                else
                    increasePosBy 3 currentState

            let newOverallState =
                state |> changeAmpState state.CurrentlyProcessing newState
            
            play newOverallState
        
        | AddMul (fstMode, sndMode, operation) ->
            let op1 = findOp2 (pos + 1) currentProgram fstMode
            let op2 = findOp2 (pos + 2) currentProgram sndMode
            let storeIdx = Map.find (pos + 3) currentProgram
            let res = operation op1 op2
            
            let newState =
                currentState
                |> modInstrAt storeIdx res
                |> increasePosBy 4

            let newOverallState =
                state |> changeAmpState state.CurrentlyProcessing newState

            play newOverallState

    let finalState = play init
    finalState.AmpStates
    |> Map.find finalState.CurrentlyProcessing
    |> (fun x -> x.NextInput.Value)
    
let ans2 = 
    Helpers.permutations [5;6;7;8;9]
    |> Seq.map (transform2 data)
    |> Seq.max

ans2