// define function to find the number of solutions for p
let countSolutions p =
    [4*p/10..6*p/10]
    |> List.filter (fun c ->
        [1..p]
        |> Seq.takeWhile (fun b -> b + c < p)
        |> Seq.exists (fun b -> (pown (p-b-c) 2 + pown b 2) = pown c 2))
    |> List.length
        
let answer = [1..1000] |> List.maxBy countSolutions