let number = [1..1000] |> List.map (fun n -> pown (bigint(n)) n) |> List.sum
 
let answer = 
    number.ToString().ToCharArray() 
    |> Array.rev
    |> Seq.take 10 
    |> Seq.toArray 
    |> Array.rev