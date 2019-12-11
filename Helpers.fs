module Helpers

open System
open System.Net
open System.IO

let getDataCustomUrl (url : string) =
    let req = WebRequest.Create(url)
    use respStr = req.GetResponse().GetResponseStream()
    use strReader = new StreamReader(respStr)
    strReader.ReadToEnd()

let getData (day : int) =
    getDataCustomUrl (sprintf "https://adventofcode.com/2019/day/%i/input" day)

let getDataLines (day : int) =
    getData day
    |> (fun d -> d.Split('\n'))

let getDataCustomUrlLines url =
    getDataCustomUrl url
    |> (fun d -> d.Split('\n'))

let permutations list =
    let rec permutations' list taken = 
      seq { if Set.count taken = List.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then 
                for perm in permutations' list (Set.add l taken)  do
                  yield l::perm }

    permutations' list Set.empty