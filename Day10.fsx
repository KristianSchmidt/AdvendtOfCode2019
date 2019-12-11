#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data =
    File.ReadAllLines("./input/10.txt")
    |> Array.map (fun s -> s.ToCharArray())

let yMax = data.Length - 1
let xMax = data.[0].Length - 1

let asteroids =
    seq {
        for x in 0 .. xMax do
            for y in 0 .. yMax do
                if (data.[y].[x] = '#') then
                    yield (x, y)
    } |> Seq.toArray

let vec ((xO, yO) : int*int) ((xD, yD) : int*int) =
    (float (xD-xO), float (yD-yO))

let angle (x,y) = Math.Atan2(x, y)

let distance p1 p2 =
    let (x,y) = vec p1 p2
    Math.Sqrt(x*x + y*y)

let setOfAsteroids = Set.ofArray asteroids

let calculateVisible asteroid =
    setOfAsteroids.Remove asteroid
    |> Set.toArray
    |> Array.groupBy (fun a' -> angle (vec asteroid a'))

let most =
    asteroids
    |> Array.maxBy calculateVisible // turns out that array comparison works on size

let visible = calculateVisible most

let ans1 = Array.length visible

ans1

/// Part 2

let arr =
    visible
    |> Array.sortByDescending fst

let ans2 =
    arr.[199]
    |> (snd >> Array.minBy (distance most))
    |> (fun (x,y) -> 100*x + y)

ans2