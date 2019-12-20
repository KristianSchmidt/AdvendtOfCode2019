#load "Helpers.fs"
#time "on"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Tile =
    | Empty
    | Wall
    | Origin
    | Key of string
    | Door of string

module Tile =
    let isNotDoorOrWall =
        function
        | Wall | Door _ -> false
        | _ -> true

    let isKey = function | Key _ -> true | _ -> false
    let getKey = function | Key k -> Some k | _ -> None

let parseTile c =
    match c with
    | '.' -> Empty
    | '#' -> Wall
    | '@' -> Origin
    | k when Char.IsLower(c) -> Key (string k)
    | k when Char.IsUpper(c) -> Door (string k)
    | _ -> failwithf "Failed to parse %s" (string c)

let readRow y (s : string) =
    s.ToCharArray()
    |> Array.mapi (fun x c -> (x,y), parseTile c)

let transformToInput (arr : string array) =
    arr
    |> Array.mapi readRow
    |> Array.collect id
    |> Map.ofArray

let data =
    File.ReadAllLines("./input/18.txt")
    |> transformToInput

let data132 =
    [| "########################"
       "#...............b.C.D.f#"
       "#.######################"
       "#.....@.a.B.c.d.A.e.F.g#"
       "########################" |]
    |> transformToInput

let data136 =
    [| "#################"
       "#i.G..c...e..H.p#"
       "########.########"
       "#j.A..b...f..D.o#"
       "########@########"
       "#k.E..a...g..B.n#"
       "########.########"
       "#l.F..d...h..C.m#"
       "#################" |]
    |> transformToInput

let data81 =
    [|
    "########################"
    "#@..............ac.GI.b#"
    "###d#e#f################"
    "###A#B#C################"
    "###g#h#i################"
    "########################"
    |] |> transformToInput

type Pos = int*int

let getAllNeighbors (x,y) =
    [| (x,y+1)
       (x,y-1)
       (x-1,y)
       (x+1,y) |]

let generateEdges graph (pos : Pos) =
    getAllNeighbors pos
    |> Array.filter (fun pos' -> Map.tryFind pos' graph
                                 |> Option.defaultValue Wall
                                 |> Tile.isNotDoorOrWall)

let removeKeyAndDoor t m =
    let k = 
        match t with
        | Key key -> key
        | _ -> failwithf "Not passed a key"

    m
    |> Map.map (fun _ v -> match v with
                           | Key k' when k' = k ->
                                //printfn "Removing %A" v
                                Empty
                           | Door d when d = k.ToUpper() ->
                                //printfn "Removing %A" v
                                Empty
                           | _ -> v)

(*
    Repeat: Until all keys are gone
    Do BFS to find out what can be searched
    Find areas with keys
    Do dijkstra to get all distances for the keys
    Heuristic: Choose the one closest
    Remove the key and door
*)

type PartialSol = { KeysLeft : Map<Pos,Tile>
                    DistanceTraveled : int
                    EstimateRest : int
                    CurrTiles : Map<Pos,Tile>
                    CurrPos : Pos
                      }

let randomize xs = xs |> List.map (fun x -> random 0 100,x) |> List.sortBy fst |> List.map snd

let takeMax n xs = List.take (min n (List.length xs)) xs

let manhattan ((p1x,p1y) : Pos) ((p2x,p2y) : Pos) =
    abs (p1x - p2x) + abs (p1y - p2y)

let distanceToKeys (keys : Map<Pos,Tile>) pos =
    keys
    |> Map.toSeq
    |> Seq.sumBy (fst >> (manhattan pos))

let solve tiles =
    let origin = tiles |> Map.findKey (fun k v -> v = Origin)
    
    let keyMap = tiles |> Map.filter (fun p t -> Tile.isKey t)
    
    let mutable iter = 0L

    let rec f (queue : PartialSol list) bestVal =
        iter <- iter + 1L
        if (iter % 1000L = 0L) then printfn "Iter: %i. Queue size: %i" iter queue.Length
        match queue with
        | [] -> printfn "Exhaused queue. %i" bestVal
        | psol :: xs ->
        if (Map.isEmpty psol.KeysLeft) then // Is a final solution
            if (psol.DistanceTraveled < bestVal) then
                printfn "New best: %i < %i" psol.DistanceTraveled bestVal
                f xs psol.DistanceTraveled
            else
                f xs bestVal
        else

            let edges = generateEdges psol.CurrTiles
            let availVertices = Helpers.BFS.bfs edges psol.CurrPos
            // The keys we can see now are the choices we have
            let availKeys = availVertices
                            |> Map.filter (fun p v -> Map.find p psol.CurrTiles |> Tile.isKey)

            let constructPsol ((pos,d) : Pos*int) =
                let chosenKey = Map.find pos psol.CurrTiles
                let newTiles = removeKeyAndDoor chosenKey psol.CurrTiles
                let newKeysLeft = psol.KeysLeft |> Map.remove pos
                let newDistance = psol.DistanceTraveled + d
                let estimate = distanceToKeys newKeysLeft pos
                { KeysLeft = newKeysLeft; DistanceTraveled = newDistance; EstimateRest = estimate; CurrTiles = newTiles; CurrPos = pos }
            
            let newPsols =
                List.map constructPsol (Map.toList availKeys)
                |> List.filter (fun psol' -> psol'.DistanceTraveled + psol'.EstimateRest < bestVal) // Needs to be lower than bestVal in order to even have a chance
                |> List.sortBy (fun psol' -> psol'.DistanceTraveled + psol'.EstimateRest)

            let newQueue = List.concat [newPsols; xs]// |> randomize
                           
            //let distance = Map.find chosenPos availVertices
            //printfn "Chose to remove %A. %A -> %A costs %i" chosenKey currPos chosenPos distance
            
            f newQueue bestVal
    
    let estimate = distanceToKeys keyMap origin
    let init = { KeysLeft = keyMap; DistanceTraveled = 0; EstimateRest = estimate; CurrTiles = tiles; CurrPos = origin }
    f [ init ] Int32.MaxValue


let nonWallNeighbors p data =
    let n = getAllNeighbors p
    let nTiles = n |> Array.map (fun k -> (Map.tryFind k data |> Option.defaultValue Wall))
    //printfn "n: %A" n
    //printfn "nTiles: %A" nTiles
    nTiles |> Array.filter ((<>)Wall) |> Array.length

let rmDeadEnd (init : Map<Pos,Tile>) =
    let rec loop (m : Map<Pos,Tile>) =
        let candidates =
            m
            |> Seq.filter (fun kv -> kv.Value = Empty && nonWallNeighbors kv.Key m = 1)
            |> Seq.map (fun kv -> kv.Key)
            |> Array.ofSeq

        if (candidates.Length > 0) then
            printfn "Removing %i tiles." candidates.Length
            let newMap = candidates |> Array.fold (fun m' p -> Map.add p Empty m') m
            loop newMap
        else
            m

    loop init
    

rmDeadEnd data






|> Array.ofSeq
|> Array.map (fun kv -> kv.Key)
|> Array.sort

nonWallNeighbors (5,1)


/// Part 2

let ans2 = data

ans2
