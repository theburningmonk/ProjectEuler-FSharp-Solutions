open System
 
let findFactorsOf(n) =
    let upperBound = int32(Math.Sqrt(double(n)))
    [2..upperBound]
    |> Seq.filter (fun x -> n % x = 0)
 
let isPrime(n) = findFactorsOf(n) |> Seq.length = 0
let primeNumbers = Seq.unfold (fun x -> Some(x, x + 1)) 2 |> Seq.filter isPrime
let p = primeNumbers |> Seq.nth(10000)