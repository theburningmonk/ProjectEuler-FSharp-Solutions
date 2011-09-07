open System.IO

let (answer, _) = 
    File.ReadAllLines(@"c:\temp\base_exp.txt") 
    |> Array.mapi (fun i l -> (i + 1, l.Split(',') |> Array.map float))
    |> Array.map (fun t -> match t with | (i, arr) -> (i, arr.[1] * (log arr.[0])))
    |> Array.maxBy (fun t -> match t with | (i, l) -> l)