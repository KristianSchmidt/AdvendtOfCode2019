#load "Helpers.fs"

#nowarn "0025"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllLines("./input/14.txt")

//let data = [| "10 ORE => 10 A"
//              "1 ORE => 1 B"
//              "7 A, 1 B => 1 C"
//              "7 A, 1 C => 1 D"
//              "7 A, 1 D => 1 E"
//              "7 A, 1 E => 1 FUEL" |]

type Resource = Res of string

let parseResCount (s : String) =
    let [|count;res|] = s.Split(' ')
    int64 count, Res res

type Recipe = { Input : (int64 * Resource) array; Output : (int64 * Resource) }
    
let recipes =
    data
    |> Array.map (split " => ")
    |> Array.map (fun arr -> split ", " arr.[0] |> Array.map parseResCount, parseResCount arr.[1])
    |> Array.map (fun (i,o) -> { Input = i; Output = o })

recipes.Length
recipes |> Array.map (fun r -> snd r.Output) |> Array.distinct |> Array.length

let recipesMap =
    recipes |> Array.map (fun r -> snd r.Output, r) |> Map.ofArray

recipesMap |> Map.find (Res "FUEL")

let addToNeeds newNeeds (m : Map<Resource,int64>) =
    let newResCount (c,res) =
        Map.tryFind res m
        |> Option.defaultValue 0L
        |> ((+)c)
        
    newNeeds
    |> Array.fold (fun m' (c,res) -> Map.add res (newResCount (c,res)) m') m

let subtractFromSurplus (c,res) (m : Map<Resource,int64>) =
    let newCount = (Map.tryFind res m |> Option.defaultValue 0L) - c
    if (newCount < 0L) then printfn "WARNING: Negative surplus %A" (c,res)
    m |> Map.add res c

let addToSurplus (c,res) (m : Map<Resource,int64>) =
    let newCount = (Map.tryFind res m |> Option.defaultValue 0L) + c
    if (newCount < 0L) then printfn "WARNING: Negative surplus %A" (c,res)
    m |> Map.add res c

let findOre (recipes : Map<Resource, Recipe>) fuelNeeded =
    let rec f (need : Map<Resource,int64>) (surplus : Map<Resource,int64>) =
        let nxtOpt =
            need
            |> Map.toList
            |> List.sortByDescending snd
            |> List.tryFind (fst >> ((<>)(Res "ORE")))
            |> Option.map fst
        
        
        match nxtOpt with
        | None -> need |> Map.find (Res "ORE")
        | Some nxt ->
            let origReqCount = Map.find nxt need
            //printf "Making %i %A" origReqCount nxt
            let surplusUsed =
                match Map.tryFind nxt surplus with
                | Some s -> min s origReqCount
                | None -> 0L
            
            let reqCount = origReqCount - surplusUsed
            
            let nxtRecipe = Map.find nxt recipes
            let recipeYield = fst nxtRecipe.Output

            let recipesToMake = // is just math.ceil
                (reqCount / recipeYield) + (int64 <| Math.Sign(reqCount % recipeYield))

            let resourceSurplus = recipeYield * recipesToMake - reqCount

            let newNeeds =
                nxtRecipe.Input
                |> Array.map (fun (k,v) -> k*recipesToMake, v)

            //printfn "\t=> %A\tSurplus: %i" newNeeds resourceSurplus

            let newNeedMap = (addToNeeds newNeeds need |> Map.remove nxt)
            let newSurplusMap =
                surplus
                |> subtractFromSurplus (surplusUsed,nxt)
                |> addToSurplus (resourceSurplus,nxt)

            f newNeedMap newSurplusMap

    f (Map.ofList [ (Res "FUEL", fuelNeeded) ]) Map.empty

let ans1 = findOre recipesMap 1L

ans1

/// Part 2

let trillion = 1_000_000_000_000L
let guess = trillion / (int64 ans1)
// TODO: Make proper binary search
let ans2 = findOre recipesMap (guess + 1_183_622L)

ans2