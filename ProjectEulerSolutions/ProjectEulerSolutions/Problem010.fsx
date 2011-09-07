open System

let findFactorsOf(n:int64) =
    let upperBound = int64(Math.Sqrt(double(n)))
    [2L..upperBound] |> Seq.filter (fun x -> n % x = 0L)
 
let isPrime(n:int64) = findFactorsOf(n) |> Seq.length = 0
 
let primeSequence max = seq { for n in 2L..max do if isPrime(n) then yield n }
let sum = primeSequence 1999999L |> Seq.sum