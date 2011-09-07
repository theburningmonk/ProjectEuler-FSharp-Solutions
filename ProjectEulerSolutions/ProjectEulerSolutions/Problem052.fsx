// check if two numbers are permutations of the same set of digits
let isPermutation number1 number2 =
    let str1 = number1.ToString().ToCharArray() |> Array.sort
    let str2 = number2.ToString().ToCharArray() |> Array.sort
 
    if str1.Length <> str2.Length then false
    else Array.forall2 (fun x y -> x = y) str1 str2
 
let answer =
    Seq.unfold (fun state -> Some(state, state + 1)) 1
    |> Seq.filter (fun n -> 
        [2..6] |> List.map (fun x -> x * n) |> List.forall (fun xn -> isPermutation xn n))
    |> Seq.head