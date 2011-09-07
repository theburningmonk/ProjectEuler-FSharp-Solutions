let fibonacciSeq = Seq.unfold (fun (current, next) -> Some(current, (next, current + next))) (0, 1)
 
let answer =
    fibonacciSeq
    |> Seq.takeWhile (fun n -> n < 4000000)
    |> Seq.filter (fun n -> n % 2 = 0)
    |> Seq.sum