let rec gcd a b = 
    if b = 0 
    then abs a 
    else gcd b (a % b)
 
let ``3/7`` = 3.0 / 7.0
 
let answer = 
    [8..1000000] 
    |> List.map (fun d -> int (floor (``3/7`` * float d)), d)
    |> List.filter (fun (n, d) -> gcd n d = 1)
    |> List.maxBy (fun (n, d) -> float n / float d)
    |> fst  // gets just the numerator