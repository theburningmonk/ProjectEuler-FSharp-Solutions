let rec factorial (n:bigint) = if n = 1I then 1I else n * factorial(n-1I)
 
let number = factorial 100I
let digits = number.ToString().ToCharArray() |> Seq.map (fun c -> int32(c.ToString()))
let sum = digits |> Seq.sum