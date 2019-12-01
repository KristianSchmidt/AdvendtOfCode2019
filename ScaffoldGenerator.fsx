open System
open System.IO

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let contents = File.ReadAllText("Template.fsx")

for i in 1 .. 25 do
    let newContents = contents.Replace("[DAY]", i.ToString())
    File.WriteAllText(sprintf "Day%i.fsx" i, newContents)