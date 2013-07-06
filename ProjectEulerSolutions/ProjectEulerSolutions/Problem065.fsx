let ``convergents of √2`` = seq { for i in 2I..2I..2000I do yield! [1I; i; 1I] }
 
let answer = 
    let numerator = 
        ``convergents of √2``
        |> Seq.skip 1
        |> Seq.take 98
        |> Seq.fold (fun (``n-1``, n) i -> (n, ``n-1`` + n * i)) (2I, 3I)
        |> snd
    
    string numerator |> Seq.map (string >> int) |> Seq.sum
