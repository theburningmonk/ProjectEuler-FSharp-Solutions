
open System.IO
 
let numbers =
    File.ReadAllLines(@"C:\TEMP\euler8.txt")
    |> Seq.concat
    |> Seq.map (fun c -> int32(c.ToString()))
 
let CalcProduct numbers = numbers |> Seq.fold (fun acc n -> acc * n) 1
 
let maxProduct =
    numbers
    |> Seq.windowed(5)
    |> Seq.map (fun n -> CalcProduct n)
    |> Seq.max