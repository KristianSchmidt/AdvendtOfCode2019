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
    let isKey = function | Key _ -> true | _ -> false
    let isKeyOrOrigin = function | Key _ | Origin -> true | _ -> false
    let getKey = function | Key k -> Some k | _ -> None

    let isKeyOrDoor = function | Key _ | Door _ -> true | _ -> false

    let isEmptyOrDoor = function | Empty | Door _ -> true | _ -> false

    let draw =
        function
        | Empty -> "."
        | Wall -> "#"
        | Origin -> "@"
        | Key k -> k
        | Door d -> d

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

let draw (m : Map<Pos,Tile>) =
    let drawRow (_,arr) =
        arr
        |> Seq.sortBy fst
        |> Seq.map snd
        |> String.concat ""
    
    m
    |> Map.map (fun k v -> Tile.draw v)
    |> Map.toSeq
    |> Seq.groupBy (fst >> snd)
    |> Seq.sortBy fst
    |> Seq.map drawRow
    |> String.concat "\n"
    |> prependNewline

let getPosOf (t : Tile) (m : Map<Pos,Tile>) =
    Map.findKey (fun k v -> v = t) m

let getAllNeighbors (x,y) =
    [| (x,y+1)
       (x,y-1)
       (x-1,y)
       (x+1,y) |]

let nonWallNeighbors p data =
    getAllNeighbors p
    |> Array.map (fun k -> (Map.tryFind k data |> Option.defaultValue Wall))
    |> Array.filter ((<>)Wall)
    |> Array.length

let rmDeadEnd (init : Map<Pos,Tile>) =
    let rec loop (m : Map<Pos,Tile>) =
        let candidates =
            m
            |> Seq.filter (fun kv -> Tile.isEmptyOrDoor kv.Value && nonWallNeighbors kv.Key m = 1)
            |> Seq.map (fun kv -> kv.Key)
            |> Array.ofSeq

        if (candidates.Length > 0) then
            //printfn "Removing %i tiles." candidates.Length
            let newMap = candidates |> Array.fold (fun m' p -> Map.add p Wall m') m
            loop newMap
        else
            m

    loop init

let adjf m pos =
    getAllNeighbors pos
    |> Array.filter (fun p -> Wall <> (Map.tryFind p m |> Option.defaultValue Wall))

let constructPath (p : Map<Pos,Pos>) dest =
    let rec loop xs d =
        match Map.tryFind d p with
        | Some parent ->
            //printfn "Next up %A" parent
            loop (parent :: xs) parent
        | None -> xs
    loop [dest] dest

let getPath m parents pos =
    constructPath parents pos
    |> List.map (fun k -> Map.find k m)
    |> List.filter (Tile.isKeyOrDoor)

let availableKeys (arr : Tile list array) =
    let findKey (xs : Tile list) =
        match List.tryHead xs with
        | Some (Door _) -> None
        | Some (Key k) -> Some k
        | Some _ -> failwithf "."
        | None -> None

    arr
    |> Array.choose findKey
    |> Array.distinct

let removeTile t (arr : Tile list array) =
    arr
    |> Array.map (List.filter ((<>)t))

type PartialSol = { Paths : Tile list array
                    DistanceTraveled : int
                    CurrentKey : string option
                    Visited : string list
                   }

let allKeysDistances (m : Map<Pos,Tile>) =
    let keyPos =
        m
        |> Map.filter (fun k v -> Tile.isKeyOrOrigin v)

    let filterToKeys o (m : Map<Pos,int>) =
        m
        |> Map.filter (fun k v -> Map.containsKey k keyPos)
        |> Map.toSeq
        |> Seq.map (fun (k,v) -> (o,Map.find k keyPos), v)
        |> Map.ofSeq
    
    let allKeys =
        m
        |> Map.filter (fun k v -> Tile.isKeyOrOrigin v)
        |> Map.toArray
        |> Array.map (fun (p,t) -> Helpers.BFS.bfs (adjf m) p
                                   |> filterToKeys t)
    
    allKeys
    |> Array.fold Map.merge Map.empty
    
let makePaths m =
    let (_, parents) =
        Helpers.BFS.bfsWithPath (adjf m) (getPosOf Origin m)
    
    m
    |> Map.filter (fun k v -> v <> Wall && nonWallNeighbors k m = 1)
    |> Map.toArray
    |> Array.map (fst >> (getPath m parents))
    |> Array.filter (List.isEmpty >> not)

let solve (m : Map<Pos,Tile>) =
    let distances = allKeysDistances m

    let paths = makePaths m
    
    let mutable iter = 0L
    let rec loop (queue : PartialSol list) bestSol =
        match queue with
        | [] -> bestSol
        | psol :: xs ->
            let keys = availableKeys psol.Paths
            let currKey =
                psol.CurrentKey
                |> Option.map (fun k -> Key k)
                |> Option.defaultValue Origin
            
            match keys with
            | [||] -> // It's a final solution
                iter <- iter + 1L
                if (iter % 1000000L = 0L) then printfn "Iter: %i" iter
                if (psol.DistanceTraveled < bestSol) then
                    printfn "New best: %A" psol
                    loop xs psol.DistanceTraveled
                else
                    loop xs bestSol
            | _ ->
                let makeChildSol k =
                    let distToChild = Map.find (currKey, Key k) distances
                    { psol with
                        Paths = psol.Paths
                                |> removeTile (Door (k.ToUpper()))
                                |> removeTile (Key k)
                        DistanceTraveled = psol.DistanceTraveled + distToChild 
                        CurrentKey = Some k
                        Visited = k :: psol.Visited
                    }
                let nextSols =
                    keys
                    |> Array.map makeChildSol
                    |> List.ofArray
                    |> List.sortBy (fun psol -> psol.DistanceTraveled)

                loop (List.concat [nextSols; xs]) bestSol
    
    loop [{ Paths = paths
            DistanceTraveled = 0
            CurrentKey = None
            Visited = List.empty }] Int32.MaxValue

solve (rmDeadEnd data)

/// Part 2

let data2 =
    let (x,y) = getPosOf Origin data
    data
    |> Map.add (x-1,y-1) Origin |> Map.add (x,y-1) Wall |> Map.add (x+1,y-1) Origin
    |> Map.add (x-1,y) Wall |> Map.add (x,y) Wall |> Map.add (x+1,y) Wall
    |> Map.add (x-1,y+1) Origin |> Map.add (x,y+1) Wall |> Map.add (x+1,y+1) Origin
    //|> Map.map (fun k v -> match v with | Door _ -> Empty | _ -> v)

let (xO,yO) = getPosOf Origin data
let ulMap = data2 |> Map.filter (fun (x,y) _ -> x <= xO && y <= yO) |> rmDeadEnd
let urMap = data2 |> Map.filter (fun (x,y) _ -> x >= xO && y <= yO) |> rmDeadEnd
let llMap = data2 |> Map.filter (fun (x,y) _ -> x <= xO && y >= yO) |> rmDeadEnd
let lrMap = data2 |> Map.filter (fun (x,y) _ -> x >= xO && y >= yO) |> rmDeadEnd

let extractCommon (paths : Tile list array) =
    let allHeadsEqual paths =
        let distinctHeads = paths |> Array.map List.tryHead |> Array.distinct
        if (distinctHeads.Length = 1 &&
            distinctHeads.[0].IsSome &&
            Tile.isKey distinctHeads.[0].Value) then
            distinctHeads.[0]
        else
            None

    let rec loop xs res =
        match allHeadsEqual xs with
        | Some k -> loop (xs |> Array.map List.tail) (k :: res)
        | None -> List.rev res

    loop paths List.empty

type PartialSol2 =
    { Paths : Tile list array array
      DistancesTraveled : int array
      CurrentKeys : string option array
    }

// Removes the common prefix from some paths
let transformPaths (paths : Tile list array) =
    let common = extractCommon paths
    let commonLength = common.Length
    paths |> Array.map (List.skip commonLength), common

(*
1. Start with some partial solution
2. Check each quardrant for any freebie paths
3. If there are paths, calculate extra distance taken and remove freebies from paths
4. For the paths with freebies removed, check available keys
5. For each of the avail keys, create a child partial solution
6. Queue up new solutions
*)

let calcExtraDistance (distances : Map<Tile*Tile,int>) (key,tiles) =
    let getDistance [x1;x2] = Map.find (x1,x2) distances
    let keyTile = match key with | None -> Origin | Some k -> Key k
    (keyTile :: tiles)
    |> List.windowed 2
    |> List.sumBy getDistance

let pathContainsKey k (paths : Tile list array) =
    paths
    |> Array.exists (List.exists ((=)(Key k)))

let removeKeyAndDoor (k : string) (paths : Tile list array array) =
    paths
    |> Array.map (Array.map (List.filter (fun t -> t <> (Door (k.ToUpper())) && t <> (Key k))))
    |> Array.map (Array.filter ((<>)List.empty))

let mapAtIdx idx f arr =
    arr
    |> Array.mapi (fun i x -> if (i = idx) then f x else x)

let rec solver2Loop distances (queue : PartialSol2 list) (bestSol : PartialSol2) =
    match queue with
    | [] -> bestSol
    | psol :: xs ->
        let (newPaths, commons) = 
            psol.Paths
            |> Array.map transformPaths
            |> Array.unzip

        let keysFoundFreebie =
            commons
            |> Array.collect (List.choose Tile.getKey >> Array.ofList)
        //printfn "Keys found: %A" keysFoundFreebie

        let newPaths =
            keysFoundFreebie
            |> Array.fold (fun arr k -> removeKeyAndDoor k arr) newPaths

        let freebieDistances = Array.zip psol.CurrentKeys commons
                               |> Array.map (calcExtraDistance distances)
                
        let distancesAfterFreebies =
            Array.zip freebieDistances psol.DistancesTraveled
            |> Array.map (fun (x1,x2) -> x1 + x2)

        let newKeys =
            let freebieKeys =
                commons
                |> Array.map List.tryLast
                |> Array.map (Option.bind Tile.getKey)
            Array.zip freebieKeys psol.CurrentKeys
            |> Array.map
                (fun (freebie,curr) -> 
                    match freebie with
                    | Some k -> Some k
                    | None -> curr
                )

        let psolAfterFreebies =
            { Paths = newPaths; DistancesTraveled = distancesAfterFreebies;
              CurrentKeys = newKeys }

        let choices = newPaths |> Array.collect availableKeys

        let makePsol k =
            let idxOfKey = newPaths |> Array.findIndex (pathContainsKey k)
            let newPaths = removeKeyAndDoor k newPaths
            let keyAtChoice =
                match newKeys.[idxOfKey] with
                | None -> Origin
                | Some curr -> Key curr

            // update distance
            let newDistances =
                distancesAfterFreebies
                |> mapAtIdx idxOfKey ((+) (Map.find (keyAtChoice, Key k) distances))
            // update key
            let newKeys = mapAtIdx idxOfKey (fun _ -> Some k) newKeys
            
            { Paths = newPaths
              DistancesTraveled = newDistances
              CurrentKeys = newKeys
            }
            
        let nextSols =
            choices
            |> Array.map makePsol
            |> List.ofArray
            |> List.sortBy (fun psol -> Array.sum psol.DistancesTraveled)

        match nextSols with
        | [] -> // It's a final solution
            
            let dist = Array.sum psolAfterFreebies.DistancesTraveled
            if (dist < (Array.sum bestSol.DistancesTraveled)) then
                printfn "New best: %i" dist
                //printfn "Choices: %A" choices
                //printfn "New paths: %A" psolAfterFreebies.Paths
                //printfn "%A" psolAfterFreebies
                solver2Loop distances xs psolAfterFreebies
            else
                solver2Loop distances xs bestSol
        | _ ->
            let nextQueue = List.concat [nextSols; xs]
            solver2Loop distances nextQueue bestSol
            //solver2Loop distances xs bestSol

let solve2 (maps : Map<Pos,Tile> array) =
    let distances =
        maps
        |> Array.map allKeysDistances
        |> Array.fold Map.merge Map.empty // Fold into a single map
    
    let startingPaths = Array.map makePaths maps
    let startingDistances = Array.replicate maps.Length 0
    let startingKeys = Array.replicate maps.Length None // None = Origin

    let init =
        { Paths = startingPaths
          DistancesTraveled = startingDistances
          CurrentKeys = startingKeys
        }
        
    solver2Loop distances [init] { init with DistancesTraveled = Array.replicate maps.Length 100_000 }

let sol = solve2 [| urMap; ulMap; lrMap; llMap |]

let ans2 = Array.sum sol.DistancesTraveled

ans2
