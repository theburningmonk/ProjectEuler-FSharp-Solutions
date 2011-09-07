let naturalNumbers = Seq.unfold (fun state -> Some(state, state+1)) 1
 
let rec cycleLength n =
    if n % 2I = 0I then cycleLength (n/2I)
    else
        if n % 5I = 0I then cycleLength (n/5I)
        else
            naturalNumbers
            |> Seq.filter (fun x -> ((pown 10I x) - 1I) % n = 0I)
            |> Seq.head
 
let answer = [1I..999I] |> Seq.maxBy cycleLength