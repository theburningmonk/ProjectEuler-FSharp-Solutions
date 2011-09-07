let naturalNumbers n = Seq.unfold (fun state -> Some(state, state + 1I)) n
 
// define the function T, P and H
let T n = n * (n + 1I) / 2I
let P n = n * (3I * n - 1I) / 2I
let H n = n * (2I * n - 1I)
 
// define the sequences for each function from the point the brief left off at
let TSeq = naturalNumbers 285I |> Seq.map T
let PSeq = naturalNumbers 165I |> Seq.map P
let HSeq = naturalNumbers 143I |> Seq.map H
 
let answer =
    HSeq
    |> Seq.skip 1
    |> Seq.filter (fun h -> PSeq |> Seq.takeWhile (fun p -> p <= h) |> Seq.exists (fun p -> p = h))
    |> Seq.filter (fun h -> TSeq |> Seq.takeWhile (fun t -> t <= h) |> Seq.exists (fun t -> t = h))
    |> Seq.head