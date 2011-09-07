open System.IO
 
let values =
    File.ReadAllLines(@"C:\TEMP\names.txt")
    |> Array.map (fun s -> s.Replace("\"", ""))
    |> Array.collect (fun s -> s.Trim().Split(','))
    |> Array.sort
    |> Array.map (fun s -> s.ToUpper().ToCharArray() |> Array.sumBy (fun c -> int32(c) - (int32('A') - 1)))
 
let answer = Array.map2 (fun v p -> v * p) values [|1..values.Length|] |> Array.sum