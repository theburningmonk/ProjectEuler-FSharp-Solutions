let hasDivisor(n) =
    let upperBound = bigint(sqrt(double(n)))
    [2I..upperBound] |> Seq.exists (fun x -> n % x = 0I)
 
let isPrime(n) = if n = 1I then false else not(hasDivisor(n))
 
let naturalNumbers = Seq.unfold (fun state -> Some(state, state+1I)) 1I
 
// define the sequence of prime numbers
let primeSeq = naturalNumbers |> Seq.filter isPrime |> Seq.cache
 
// recursive function to find the prime denominators for a number n
let rec getPrimeFactors denominators n =
    if n = 1I then denominators
    else
        let denominator = primeSeq |> Seq.filter (fun x -> n % x = 0I) |> Seq.head
        getPrimeFactors (denominators @ [denominator]) (n/denominator)
 
// curry the getPrimeDenominators function to start with an empty list
let primeFactors = getPrimeFactors []
 
// function to get the number of distinct prime factors a number has
let distinctPrimeFactorsCount n = primeFactors n |> Seq.distinct |> Seq.length
 
// define the sequence of numbers with exactly 4 distinct prime factors
let seq = naturalNumbers |> Seq.filter (fun n -> distinctPrimeFactorsCount n = 4) |> Seq.cache
 
let answer =
    seq
    |> Seq.windowed 4
    |> Seq.filter (fun l -> Seq.max (l) - Seq.min (l) = 3I)
    |> Seq.head
    |> Seq.head