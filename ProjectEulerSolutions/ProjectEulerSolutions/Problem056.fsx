let digitalSum n = n.ToString().ToCharArray() |> Array.map (fun c -> int(c.ToString())) |> Array.sum
 
let answer =
    [1I..100I]
    |> List.collect (fun a -> [1..100] |> List.map (fun b -> digitalSum(pown a b)))
    |> List.max