let isPandigital(n) =
    let upperBound = int32(sqrt(double(n)))
 
    [2..upperBound]
    |> Seq.filter (fun x -> n % x = 0)
    |> Seq.map (fun x -> x.ToString()+(n/x).ToString()+n.ToString())
    |> Seq.exists (fun str -> 
        [1..9]
        |> List.map (fun n -> n.ToString())
        |> List.forall (fun n -> str.Contains(n) && str.IndexOf(n) = str.LastIndexOf(n)))
 
let answer = [1000..9999] |> List.filter isPandigital |> List.sum