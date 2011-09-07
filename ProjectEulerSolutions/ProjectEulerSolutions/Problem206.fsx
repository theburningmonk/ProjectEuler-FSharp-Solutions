let predicate n =
    let nStr = n.ToString()
    // make sure n is a 19 digit number
    if nStr.Length <> 19 then false
    // check the number matches the pattern 1_2_3_4_5_6_7_8_9_0
    else [1..10] |> List.forall (fun i -> int(nStr.[2*(i-1)].ToString()) = i % 10)
 
// wonderful bit of brute force CPU..
let answer =
    let mutable n = 1000000000L
    let mutable squareRoot = 0L
    while squareRoot = 0L do
        let square = n*n
        if predicate square then squareRoot <- n else n <- n+1L
 
    squareRoot
