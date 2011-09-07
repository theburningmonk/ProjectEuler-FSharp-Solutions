let isEvenlyDivided(n, m) = n % m = 0
let isEvenlyDividedByAll(n, numbers) = numbers |> Seq.forall (fun x -> isEvenlyDivided(n, x))
 
let findSmallestCommonMultiple(numbers) =
    let max = Array.max(numbers)
    Seq.unfold (fun x -> Some(x, x + 1)) 1
    |> Seq.map (fun x -> x * max)
    |> Seq.filter (fun x -> isEvenlyDividedByAll(x, numbers))
    |> Seq.head
 
let commonMultiplier = findSmallestCommonMultiple [|1..20|]