let naturalNumbers = Seq.unfold (fun state -> Some(state, state+1)) 1

// define function to find the number of n digit numbers which are also nth power
let f n =
    naturalNumbers
    |> Seq.map (fun n' -> pown (bigint(n')) n)
    |> Seq.skipWhile (fun n' -> n'.ToString().Length < n)
    |> Seq.takeWhile (fun n' -> n'.ToString().Length = n)
    |> Seq.length

let answer = 
    naturalNumbers
    |> Seq.map f
    |> Seq.takeWhile (fun l -> l > 0)
    |> Seq.sum