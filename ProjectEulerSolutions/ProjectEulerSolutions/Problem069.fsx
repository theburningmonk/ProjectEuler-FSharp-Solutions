let hasDivisor(n) =
    let upperBound = bigint(sqrt(double(n)))
    [2I..upperBound] |> Seq.exists (fun x -> n % x = 0I)
 
let isPrime(n) = if n = 1I then false else not(hasDivisor(n))
 
// define the sequence of prime numbers
let primeSeq =
    Seq.unfold (fun state -> if state >= 3I then Some(state, state+2I) else Some(state, state+1I)) 1I
    |> Seq.filter isPrime
    |> Seq.cache
 
// define function to find the prime denominators for a number n
let getPrimeFactors n =
    let rec getPrimeFactorsRec denominators n =
        if n = 1I then denominators
        else
            let denominator = primeSeq |> Seq.filter (fun x -> n % x = 0I) |> Seq.head
            getPrimeFactorsRec (denominators @ [denominator]) (n/denominator)
    getPrimeFactorsRec [] n
 
let totient n =
    let primeFactors = getPrimeFactors n |> Seq.distinct
    n * (primeFactors |> Seq.map (fun n' -> n'-1I) |> Seq.reduce (*)) / (primeFactors |> Seq.reduce (*))
 
let answer = [2I..1000000I] |> List.maxBy (fun n -> double(n) / double(totient n))