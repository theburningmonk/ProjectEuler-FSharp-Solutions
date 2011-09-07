// define function which checks if a given number is 1-9 pandigital
let isPandigital n =
    let str = n.ToString()
    [1..9]
    |> List.map string
    |> List.forall (fun x -> str.Contains(x) && str.IndexOf(x) = str.LastIndexOf(x))
 
// define function to generate the (possible) pandigital sequence
let getConcatProduct n =
    // define recursive inner function
    let rec genSeq n' (digits:string) n=
        let digits' = digits + (n' * n).ToString()
        if digits'.Length > 9 then digits else genSeq (n'+1) digits' n
    genSeq 1 "" n
 
let answer =
    [1..10000]
    |> List.map getConcatProduct
    |> List.filter (fun d -> d.Length = 9 && isPandigital d)
    |> List.maxBy int