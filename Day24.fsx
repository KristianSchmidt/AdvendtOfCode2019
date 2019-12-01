#load "Helpers.fs"

open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let data = File.ReadAllLines("./input/24.txt")

let ans1 = data

ans1

/// Part 2

let ans2 = data

ans2