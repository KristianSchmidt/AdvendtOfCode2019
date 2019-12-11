#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllText("./input/8.txt").TrimEnd().ToCharArray()
           |> Array.map (string >> int)

let width = 25
let height = 6
let size = width*height

let layerCount = data.Length / size

let splitIntoLayers (arr : int array) =
    arr |> Array.splitInto layerCount

let amountOfZeroDigits arr = arr |> Array.filter ((=)0) |> Array.length

let mi = splitIntoLayers data
         |> Array.minBy amountOfZeroDigits
         |> Array.countBy id
         |> Map.ofArray

let ans1 = (Map.find 1 mi) * (Map.find 2 mi)

ans1

/// Part 2

let pixels =
    seq {
        for y in 0 .. (height - 1) do
            for x in 0 .. (width - 1) do
                yield (x,y)
    }
    |> Seq.toArray

let getDataForPixel (x,y) =
    seq {
      for l in 0 .. (layerCount - 1) do
          let offSet = l * size
          //printfn "(%i,%i), offset=%i, pos=%i" x y offSet (offSet + (y * width) + x)
          yield data.[offSet + (y * width) + x]
    } |> Seq.find ((<>)2)

let draw i = if i=0 then "█" else "░"

let ans2 = 
    pixels
    |> Array.map (getDataForPixel >> draw)
    |> Array.splitInto (size / width)
    |> Array.map (String.concat "")
    |> String.concat "\n"

printfn "\n%s\n" ans2
