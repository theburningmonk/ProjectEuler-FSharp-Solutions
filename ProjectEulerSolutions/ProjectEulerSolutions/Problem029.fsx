let getCombos a b = [2I..a] |> List.collect (fun x -> [2..b] |> List.map (fun y -> (x, y)))
 
let answer =
    getCombos 100I 100
    |> List.map (fun (a, b) -> pown a b)
    |> List.sort
    |> Seq.distinct
    |> Seq.length