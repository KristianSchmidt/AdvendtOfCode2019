#load "Helpers.fs"

#nowarn "0025"
#nowarn "0667"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllText("./input/15.txt").Split(',')
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

type Tile = | Wall
            | Empty
            | Unknown
            | OxygenSystem
            | Origin

type AmpState =
    { Pos : int64
      RelativeBase : int64
      Instructions : Map<int64,int64>
      CurrentPos : int*int
      DesiredPos : int*int
      KnownTiles : Map<int*int, Tile>
      IsDone : bool
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

let getNonWalls pos m =
    getAllNeighbors pos
    |> List.map (fun (k, p) -> k, Map.tryFind p m |> Option.defaultValue Unknown)
    |> List.filter (snd >> ((<>)Wall))

let setCurrentPos pos ampState = { ampState with CurrentPos = pos }
let setDesiredPos pos ampState = { ampState with DesiredPos = pos }
let setKnownTiles tiles ampState = { ampState with KnownTiles = tiles }
let setDone ampState = { ampState with IsDone = true }

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
    if (state.IsDone) then None
    else

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
        
        let input = 
            getNonWalls state.CurrentPos state.KnownTiles
            |> List.tryFind (snd >> ((=)Unknown))
            |> Option.map fst
            |> Option.defaultValue (int64 <| random 1 4)
            
        //printfn "Input: %i Curr: %A" input state.CurrentPos

        let newState =
            state
            |> increasePosBy 2L
            |> modInstrAt storeIdx input
            |> setDesiredPos (getNextPos state.CurrentPos input)
            
        Some (None, newState)
    | Output fstMode ->
        let output = findReadIdx (pos + 1L) fstMode
        
        let newState = increasePosBy 2L state

        //printfn "Known tiles: %i" state.KnownTiles.Count

        match output with
        | 0L -> // hit wall
            let knownTiles = state.KnownTiles |> Map.add state.DesiredPos Wall
            Some(None, { newState with KnownTiles = knownTiles })
        | 1L -> // Empty tile
            let knownTiles = state.KnownTiles |> Map.add state.DesiredPos Empty
            Some(None, newState |> setCurrentPos state.DesiredPos |> setKnownTiles knownTiles)
        | 2L ->
            let knownTiles = state.KnownTiles |> Map.add state.DesiredPos OxygenSystem
            printfn "Found the tile: %A" state.DesiredPos // Found the tile!
            Some(Some knownTiles,
                 newState
                 |> setCurrentPos state.DesiredPos
                 |> setKnownTiles knownTiles
                 |> setDone)
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
                 DesiredPos = (0,0)
                 KnownTiles = Map.ofList [((0,0), Origin)]
                 IsDone = false
                 }

    Array.unfold play init
    |> Array.choose id

#time "on"
let coords =
    let init = runUntilHalt data |> Array.head |> Map.toArray

    let xMin, xMax, yMin, yMax = getMinMax init
    
    init |> Array.map (fun ((x,y),t) -> ((x+abs(xMin), y+abs(yMin)),t))

let xMin, xMax, yMin, yMax = getMinMax coords

let draw arr =
    let asMap = Map.ofArray arr
    let getElem x y = Map.tryFind (x,y) asMap |> Option.defaultValue Unknown
    let render = function | Wall -> "#" | Empty -> "." | OxygenSystem -> "S" | Unknown -> " " | Origin -> "O"
    let makeRow y =
        [| xMin .. xMax |]
        |> Array.map (fun x -> getElem x y |> render)
        |> String.concat "\t"
    
    [| yMin .. yMax |]
    |> Array.map makeRow
    |> String.concat "\n"
    |> prependNewline
    |> (fun s -> File.WriteAllText(@"C:\Code\AdvendtOfCode2019\out15.txt", s))
    
draw coords

let ans1 = data

ans1

/// Part 2

let rec play2 state =
    if (state.KnownTiles.Count = 1500) then
        state.KnownTiles
    else


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
    | Halt -> state.KnownTiles
    | Input inputMode ->
        let storeIdx = findStoreIdx (pos + 1L) inputMode
        
        let input = 
            getNonWalls state.CurrentPos state.KnownTiles
            |> List.tryFind (snd >> ((=)Unknown))
            |> Option.map fst
            |> Option.defaultValue (int64 <| random 1 4)
            
        //printfn "Input: %i Curr: %A" input state.CurrentPos

        let newState =
            state
            |> increasePosBy 2L
            |> modInstrAt storeIdx input
            |> setDesiredPos (getNextPos state.CurrentPos input)
            
        play2 newState
    | Output fstMode ->
        let output = findReadIdx (pos + 1L) fstMode
        
        let newState = increasePosBy 2L state

        printfn "Known tiles: %i" state.KnownTiles.Count

        let knownTiles =
            match output with
            | 0L -> // hit wall
                state.KnownTiles |> Map.add state.DesiredPos Wall
            | 1L ->
                state.KnownTiles |> Map.add state.DesiredPos Empty
            | 2L ->
                state.KnownTiles |> Map.add state.DesiredPos OxygenSystem

        match output with
        | 0L -> play2 (newState |> setKnownTiles knownTiles)
        | 1L | 2L ->
            play2 (newState
                   |> setCurrentPos state.DesiredPos
                   |> setKnownTiles knownTiles)
    | AdjustRelativeBase fstMode ->
        let op1 = findReadIdx (pos + 1L) fstMode
        
        let newState =
            state
            |> adjustRelativeBase op1
            |> increasePosBy 2L

        play2 newState
    | EqLessThan ((fstMode, sndMode, outMode), operation) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let op3 = findStoreIdx (pos + 3L) outMode
        
        let newState =
            state
            |> increasePosBy 4L
            |> modInstrAt op3 (if (operation op1 op2) then 1L else 0L)
            
        play2 newState
    | JumpIfTrueFalse ((fstMode, sndMode), test) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let newState = 
            if (test op1) then
                setPosTo op2 state
            else
                increasePosBy 3L state

        play2 newState
    
    | AddMul ((fstMode, sndMode, outMode), operation) ->
        let op1 = findReadIdx (pos + 1L) fstMode
        let op2 = findReadIdx (pos + 2L) sndMode
        let storeIdx = findStoreIdx (pos + 3L) outMode

        let res = operation op1 op2
        
        let newState =
            state
            |> modInstrAt storeIdx res
            |> increasePosBy 4L

        play2 newState

let runUntilHalt2 program =
    let init = { Pos = 0L
                 Instructions = program
                 RelativeBase = 0L
                 CurrentPos = (0,0)
                 DesiredPos = (0,0)
                 KnownTiles = Map.ofList [((0,0), Origin)]
                 IsDone = false
                 }
    
    play2 init

let res = runUntilHalt2 data

let generateGraph (m : Map<int*int,Tile>) : Map<(int*int)*(int*int),bool> =
    let generateEdges (pos : (int*int)) =
        getAllNeighbors pos
        |> Array.ofList
        |> Array.map snd
        |> Array.map (fun n -> (pos,n), Map.tryFind n m |> Option.defaultValue Wall)
        |> Array.filter (snd >> ((<>)Wall))
        |> Array.map (fun (k,_) -> k,true)
        
    m
    |> Map.toArray
    |> Array.filter (snd >> ((<>)Wall))
    |> Array.collect (fst >> generateEdges)
    |> Map.ofArray

let graph = generateGraph res

let source = res |> Map.tryFindKey (fun k v -> v = OxygenSystem) |> Option.get

Helpers.Dijkstra.dijkstra graph source
|> Map.toArray
|> Array.maxBy snd

let ans2 = data

ans2