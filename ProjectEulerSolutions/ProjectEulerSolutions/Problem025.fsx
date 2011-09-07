let fibonacciSeq =
    Seq.unfold (fun (current, next) -> Some(current, (next, current + next))) (0I, 1I)
    |> Seq.filter (fun f -> f > 0I)
 
let answer = (Seq.findIndex (fun f -> f.ToString().Length >= 1000) fibonacciSeq) + 1