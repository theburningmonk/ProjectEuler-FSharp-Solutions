// define function to return all the numerator-denominator pairs for the first n expand
let expand n =
    Seq.unfold (fun (num, denom) -> Some((num, denom), (denom*2I+num, denom+num))) (3I, 2I)
    |> Seq.take n
 
let answer =
    expand 1000
    |> Seq.filter (fun (num, denom) -> num.ToString().Length > denom.ToString().Length)
    |> Seq.length