#load "Helpers.fs"

#time "on"

open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

type Vec3 = { X : int; Y : int; Z : int }

let add vec1 vec2 = { X = vec1.X + vec2.X
                      Y = vec1.Y + vec2.Y
                      Z = vec1.Z + vec2.Z }

//let data = 
//    [| { X = -1; Y = 0;   Z = 2}
//       { X =  2; Y = -10; Z = -7}
//       { X =  4; Y = -8;  Z = 8}
//       { X =  3; Y = 5;   Z = -1}
//       
//    |]

//let data = 
//    [| { X = -8; Y = 10;   Z = 0}
//       { X =  5; Y = 5; Z = 10}
//       { X =  2; Y = -7;  Z = 3}
//       { X =  9; Y = -8;   Z = -3}
//       
//    |]

let data =
    let readLine s =
        let mc = Regex.Matches(s, "<x=(-?\d+), y=(-?\d+), z=(-?\d+)>")
        { X = mc.[0].Groups.[1].Value |> int
          Y = mc.[0].Groups.[2].Value |> int
          Z = mc.[0].Groups.[3].Value |> int }
    
    File.ReadAllLines("./input/12.txt")
    |> Array.map readLine

type MoonState = { Pos : Vec3; Vel : Vec3 }

let updatePos (ms : MoonState) =
    { ms with Pos = add ms.Pos ms.Vel}

let initState =
    data
    |> Array.mapi (fun i pos -> i, { Pos = pos; Vel = { X = 0; Y = 0; Z = 0 } })
    |> Map.ofArray

let allPairs =
    seq {
        for i in 0 .. data.Length - 1 do
            for j in i + 1 .. data.Length - 1 do
                yield (i,j)
    } |> Array.ofSeq

let velocityChange i j =
    { X = Math.Sign(j.Pos.X - i.Pos.X)
      Y = Math.Sign(j.Pos.Y - i.Pos.Y)
      Z = Math.Sign(j.Pos.Z - i.Pos.Z) }

let velocityChanges ((i, is) : int*MoonState) ((j, js) : int*MoonState) =
    [| (i, velocityChange is js); (j, velocityChange js is) |]

let updateVel (state : Map<int,MoonState>) ((i, delta) : int*Vec3) =
    let iState = Map.find i state
    let newState = { iState with Vel = add iState.Vel delta }
    state
    |> Map.add i newState

let updateState (state : Map<int, MoonState>) =
    let dv =
        allPairs
        |> Array.collect (fun (i,j) -> velocityChanges (i, Map.find i state) (j, Map.find j state))

    let withNewVel = Array.fold updateVel state dv
    withNewVel |> Map.map (fun _ v -> updatePos v)

let pot (ms : MoonState) =
    abs ms.Pos.X + abs ms.Pos.Y + abs ms.Pos.Z

let kin (ms : MoonState) =
    abs ms.Vel.X + abs ms.Vel.Y + abs ms.Vel.Z

let total (ms : MoonState) = pot ms * kin ms

let ans1 = [| 1 .. 1000 |]
           |> Array.fold (fun s _ -> updateState s) initState
           |> Map.toArray
           |> Array.sumBy (snd >> total)

ans1

/// Part 2

type AllPositions =
    { X1 : int16; Y1 : int16; Z1 : int16
      X2 : int16; Y2 : int16; Z2 : int16
      X3 : int16; Y3 : int16; Z3 : int16
      X4 : int16; Y4 : int16; Z4 : int16
    }

type AllVelocities =
    { vX1 : sbyte; vY1 : sbyte; vZ1 : sbyte
      vX2 : sbyte; vY2 : sbyte; vZ2 : sbyte
      vX3 : sbyte; vY3 : sbyte; vZ3 : sbyte
      vX4 : sbyte; vY4 : sbyte; vZ4 : sbyte
    }

type AllMoons = AllPositions * AllVelocities

let update ((m,v) : AllMoons) =
    let gX1 = sbyte ((Math.Sign(m.X2 - m.X1)) + (Math.Sign(m.X3 - m.X1)) + (Math.Sign(m.X4 - m.X1)))
    let gX2 = sbyte ((Math.Sign(m.X1 - m.X2)) + (Math.Sign(m.X3 - m.X2)) + (Math.Sign(m.X4 - m.X2)))
    let gX3 = sbyte ((Math.Sign(m.X2 - m.X3)) + (Math.Sign(m.X1 - m.X3)) + (Math.Sign(m.X4 - m.X3)))
    let gX4 = sbyte ((Math.Sign(m.X2 - m.X4)) + (Math.Sign(m.X3 - m.X4)) + (Math.Sign(m.X1 - m.X4)))
    let gY1 = sbyte ((Math.Sign(m.Y2 - m.Y1)) + (Math.Sign(m.Y3 - m.Y1)) + (Math.Sign(m.Y4 - m.Y1)))
    let gY2 = sbyte ((Math.Sign(m.Y1 - m.Y2)) + (Math.Sign(m.Y3 - m.Y2)) + (Math.Sign(m.Y4 - m.Y2)))
    let gY3 = sbyte ((Math.Sign(m.Y2 - m.Y3)) + (Math.Sign(m.Y1 - m.Y3)) + (Math.Sign(m.Y4 - m.Y3)))
    let gY4 = sbyte ((Math.Sign(m.Y2 - m.Y4)) + (Math.Sign(m.Y3 - m.Y4)) + (Math.Sign(m.Y1 - m.Y4)))
    let gZ1 = sbyte ((Math.Sign(m.Z2 - m.Z1)) + (Math.Sign(m.Z3 - m.Z1)) + (Math.Sign(m.Z4 - m.Z1)))
    let gZ2 = sbyte ((Math.Sign(m.Z1 - m.Z2)) + (Math.Sign(m.Z3 - m.Z2)) + (Math.Sign(m.Z4 - m.Z2)))
    let gZ3 = sbyte ((Math.Sign(m.Z2 - m.Z3)) + (Math.Sign(m.Z1 - m.Z3)) + (Math.Sign(m.Z4 - m.Z3)))
    let gZ4 = sbyte ((Math.Sign(m.Z2 - m.Z4)) + (Math.Sign(m.Z3 - m.Z4)) + (Math.Sign(m.Z1 - m.Z4)))
    let (vX1',vX2',vX3',vX4') = v.vX1 + gX1, v.vX2 + gX2, v.vX3 + gX3, v.vX4 + gX4
    let (vY1',vY2',vY3',vY4') = v.vY1 + gY1, v.vY2 + gY2, v.vY3 + gY3, v.vY4 + gY4
    let (vZ1',vZ2',vZ3',vZ4') = v.vZ1 + gZ1, v.vZ2 + gZ2, v.vZ3 + gZ3, v.vZ4 + gZ4
    let (x1', x2', x3', x4') = m.X1 + (int16 vX1'), m.X2 + (int16 vX2'), m.X3 + (int16 vX3'), m.X4 + (int16 vX4')
    let (y1', y2', y3', y4') = m.Y1 + (int16 vY1'), m.Y2 + (int16 vY2'), m.Y3 + (int16 vY3'), m.Y4 + (int16 vY4')
    let (z1', z2', z3', z4') = m.Z1 + (int16 vZ1'), m.Z2 + (int16 vZ2'), m.Z3 + (int16 vZ3'), m.Z4 + (int16 vZ4')
    let m' = { X1 = x1'; Y1 = y1'; Z1 = z1'
               X2 = x2'; Y2 = y2'; Z2 = z2'
               X3 = x3'; Y3 = y3'; Z3 = z3'
               X4 = x4'; Y4 = y4'; Z4 = z4' }
    let v' = { vX1 = vX1'; vY1 = vY1'; vZ1 = vZ1'
               vX2 = vX2'; vY2 = vY2'; vZ2 = vZ2'
               vX3 = vX3'; vY3 = vY3'; vZ3 = vZ3'
               vX4 = vX4'; vY4 = vY4'; vZ4 = vZ4' }
    (m',v')

let init =
    // <x=6, y=-2, z=-7>
    // <x=-6, y=-7, z=-4>
    // <x=-9, y=11, z=0>
    // <x=-3, y=-4, z=6>
    let m = { X1 = 6s;  Y1 = -2s; Z1 = -7s;
              X2 = -6s; Y2 = -7s; Z2 = -4s;
              X3 = -9s; Y3 = 11s; Z3 =  0s; 
              X4 = -3s; Y4 = -4s;  Z4 = 6s;
            }
    let v = { vX1 = 0y; vY1 = 0y; vZ1 = 0y
              vX2 = 0y; vY2 = 0y; vZ2 = 0y
              vX3 = 0y; vY3 = 0y; vZ3 = 0y
              vX4 = 0y; vY4 = 0y; vZ4 = 0y }
    m,v

let init4686 =
    // <x=-8, y=-10, z=0>
    // <x=5, y=5, z=10>
    // <x=2, y=-7, z=3>
    // <x=9, y=-8, z=-3>
    let m = { X1 = -8s;  Y1 = -10s; Z1 =   0s;
              X2 =  5s;  Y2 =   5s; Z2 =  10s;   
              X3 =  2s;  Y3 =  -7s; Z3 =   3s;   
              X4 =  9s;  Y4 =  -8s;  Z4 = -3s; 
            }
    let v = { vX1 = 0y; vY1 = 0y; vZ1 = 0y
              vX2 = 0y; vY2 = 0y; vZ2 = 0y
              vX3 = 0y; vY3 = 0y; vZ3 = 0y
              vX4 = 0y; vY4 = 0y; vZ4 = 0y }
    m,v

let init2772 =
    let m = { X1 = -1s; Y1 =   0s;   Z1 =  2s ; 
              X2 =  2s; Y2 = -10s;   Z2 = -7s; 
              X3 =  4s; Y3 =  -8s;   Z3 =  8s ; 
              X4 =  3s; Y4 =   5s;   Z4 = -1s; }
    let v = { vX1 = 0y; vY1 = 0y; vZ1 = 0y
              vX2 = 0y; vY2 = 0y; vZ2 = 0y
              vX3 = 0y; vY3 = 0y; vZ3 = 0y
              vX4 = 0y; vY4 = 0y; vZ4 = 0y }
    m,v

let getPeriod init stateFunc =
    let rec f state i seenStates =
        let vals = stateFunc state
        
        if (Set.contains vals seenStates) then
            i
        else
            let newState = update state
            f newState (i + 1L) (Set.add vals seenStates)

    f init 0L Set.empty

let xPeriod = getPeriod init (fun (p,v) -> (p.X1, p.X2, p.X3, p.X4, v.vX1, v.vX2, v.vX3, v.vX4))
let yPeriod = getPeriod init (fun (p,v) -> (p.Y1, p.Y2, p.Y3, p.Y4, v.vY1, v.vY2, v.vY3, v.vY4))
let zPeriod = getPeriod init (fun (p,v) -> (p.Z1, p.Z2, p.Z3, p.Z4, v.vZ1, v.vZ2, v.vZ3, v.vZ4))

let ans2 = Helpers.lcm (Helpers.lcm xPeriod yPeriod) zPeriod

ans2