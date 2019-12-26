[<AutoOpen>]
module Helpers

#nowarn "0025"

open System
open System.Collections.Generic

let permutations list =
    let rec permutations' list taken = 
      seq { if Set.count taken = List.length list then yield [] else
            for l in list do
              if not (Set.contains l taken) then 
                for perm in permutations' list (Set.add l taken)  do
                  yield l::perm }

    permutations' list Set.empty

let rec gcd x y = if y = 0L then abs x else gcd y (x % y)

let lcm x y = x * y / (gcd x y)

let extGCD (a : bigint) (b : bigint) =
   let rec inner (r'', s'', t'') (r', s', t') = 
       let step () = 
           let q = r'' / r'
           let r = r'' - q*r'
           let s = s'' - q*s'
           let t = t'' - q*t'
           (r, s, t)
       if r' = 0I then (r'', s'', t'')
       else inner (r', s', t') (step())

   inner (a, 1I, 0I) (b, 0I, 1I)

let inverseMod a n =
    let (gcd,x,_) = extGCD a n
    (x % n + n) % n

let memoize fn =
  let cache = new System.Collections.Generic.Dictionary<_,_>()
  (fun x ->
    match cache.TryGetValue x with
    | true, v -> v
    | false, _ -> let v = fn (x)
                  cache.Add(x,v)
                  v)

let split (splitOn : string) (s : string) = s.Split([|splitOn|], StringSplitOptions.None)

let prependNewline (s : string) =
    let sb = System.Text.StringBuilder()
    sb.AppendLine() |> ignore
    sb.Append(s) |> ignore
    sb.ToString()

let inline toMap kvps =
    kvps
    |> Seq.map (|KeyValue|)
    |> Map.ofSeq

let r = new Random()
/// Random function, inclusive of both bounds
let random min max = r.Next(min, max+1)

let getMinMax arr =
    let xs = arr |> Array.map (fst >> fst)
    let ys = arr |> Array.map (fst >> snd)
    Array.min xs, Array.max xs, Array.min ys, Array.max ys

module Map =
    let merge map1 map2 =
        Map.fold (fun acc key value -> Map.add key value acc) map1 map2

module Dijkstra =
    let private minDistance (dist : int array) (sptSet : bool array) =
        [| 0 .. (dist.Length - 1) |]
        |> Array.filter (fun i -> sptSet.[i] = false)
        |> Array.minBy (fun i -> dist.[i])
    
    let dijkstra (graph : Map<(int*int)*(int*int),bool>) (src : int*int) = 
        let vertices = graph
                       |> Map.toArray
                       |> Array.collect (fun ((k1,k2),_) -> [|k1;k2|])
                       |> Array.distinct
                       |> Array.sort
        
        let vMap = vertices |> Array.mapi (fun i e -> e,i) |> Map.ofArray
        let iMap = vertices |> Array.mapi (fun i e -> i,e) |> Map.ofArray
        let edgeFromIdx v = Map.find v iMap
        let hasEdge u v =
            let uE = edgeFromIdx u
            let vE = edgeFromIdx v
            Map.tryFind (uE,vE) graph |> Option.defaultValue false
        let V = vertices.Length
        printfn "V: %i" V

        // The output array. dist[i] 
        // will hold the shortest 
        // distance from src to i 
        let dist = Array.replicate V Int32.MaxValue

        // sptSet[i] will true if vertex 
        // i is included in shortest path 
        // tree or shortest distance from 
        // src to i is finalized 
        let sptSet = Array.replicate V false 
  
        // Distance of source vertex 
        // from itself is always 0
        let srcIdx = Map.find src vMap
        dist.[srcIdx] <- 0
  
        // Find shortest path for all vertices 
        for count in 0 .. V - 1 do 
            // Pick the minimum distance vertex 
            // from the set of vertices not yet 
            // processed. u is always equal to 
            // src in first iteration. 
            let u = minDistance dist sptSet 
  
            // Mark the picked vertex as processed 
            sptSet.[u] <- true 
  
            // Update dist value of the adjacent 
            // vertices of the picked vertex. 
            for v in 0 .. (V - 1) do
                let isNotProcessed = sptSet.[v] = false
                let newDist = dist.[u] + 1
                if (isNotProcessed && hasEdge u v && dist.[u] <> Int32.MaxValue
                    && newDist < dist.[v]) then
                    // Update dist[v] only if is not in 
                    // sptSet, there is an edge from u 
                    // to v, and total weight of path 
                    // from src to v through u is smaller 
                    // than current value of dist[v]
                    dist.[v] <- newDist
                    
        [| 0 .. V-1 |]
        |> Array.map (fun i -> edgeFromIdx i, dist.[i])
        |> Map.ofArray

module BFS =
    let bfs (adj : ('a) -> ('a) array) start =
        let q = Queue<'a>()
        q.Enqueue(start)
        let discovered = Dictionary<'a,int>()
        discovered.Add(start, 0)
        let dist pos = match discovered.TryGetValue(pos) with
                       | (true,y) -> y
                       | (false,_) -> 0

        while (q.Count > 0) do
            let v = q.Dequeue()
            adj v
            |> Array.iter (fun w -> if (not <| discovered.ContainsKey(w)) then
                                        discovered.Add(w,(dist v)+1) |> ignore
                                        q.Enqueue(w))

        toMap discovered

    let bfsWithStop (adj : ('a) -> ('a) array) start stop =
        let q = Queue<'a>()
        q.Enqueue(start)
        let discovered = Dictionary<'a,int>()
        discovered.Add(start, 0)
        let dist pos = match discovered.TryGetValue(pos) with
                       | (true,y) -> y
                       | (false,_) -> 0

        while (q.Count > 0) do
            let v = q.Dequeue()
            let adjs = adj v
            adjs
            |> Array.iter (fun w -> if (not <| discovered.ContainsKey(w)) then
                                        discovered.Add(w,(dist v)+1) |> ignore
                                        q.Enqueue(w))
            if (Array.contains stop adjs) then q.Clear()

        toMap discovered

    let bfsWithPath (adj : (int*int) -> (int*int) array) start =
        let q = Queue<int*int>()
        q.Enqueue(start)
        let discovered = Dictionary<int*int,int>()
        discovered.Add(start, 0)
        let parent = Dictionary<int*int,int*int>()
        let dist pos = match discovered.TryGetValue(pos) with
                       | (true,y) -> y
                       | (false,_) -> 0

        while (q.Count > 0) do
            let v = q.Dequeue()
            adj v
            |> Array.iter (fun w -> if (not <| discovered.ContainsKey(w)) then
                                        discovered.Add(w,(dist v)+1) |> ignore
                                        parent.Add(w,v)
                                        q.Enqueue(w))

        toMap discovered, toMap parent
    