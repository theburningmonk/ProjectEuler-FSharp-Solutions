// generate all prime numbers under <= this max
let max = 10000I

// initialise the list with 2 which is the only even number in the sequence
let mutable primeNumbers = [2I]

// only check the prime numbers which are <= the square root of the number n
let hasDivisor n =
    primeNumbers
    |> Seq.takeWhile (fun n' -> n' <= bigint(sqrt(double(n))))
    |> Seq.exists (fun n' -> n % n' = 0I)

// only check odd numbers <= max
let potentialPrimes = Seq.unfold (fun n -> if n > max then None else Some(n, n+2I)) 3I

// populate the prime numbers list
for n in potentialPrimes do if not(hasDivisor n) then primeNumbers <- primeNumbers @ [n]

// use the same hasDivisor function instead of the prime numbers list as it offers
// far greater coverage as the number n is square rooted so this function can
// provide a valid test up to max*max
let isPrime n = if n = 1I then false else not(hasDivisor(n))

// define function to find the prime denominators for a number n
let getPrimeFactors n =
    let rec getPrimeFactorsRec denominators n =
        if n = 1I then denominators
        else
            let denominator = primeNumbers |> Seq.filter (fun x -> n % x = 0I) |> Seq.head
            getPrimeFactorsRec (denominators @ [denominator]) (n/denominator)

    getPrimeFactorsRec [] n

// define Euler's totient function
let totient n =
    if n = 1I then 1I
    else if isPrime n then n-1I
    else
        let primeFactors = getPrimeFactors n |> Seq.distinct
        n * (primeFactors |> Seq.map (fun n' -> n'-1I) |> Seq.reduce (*)) / (primeFactors |> Seq.reduce (*))

// define function to check if two numbers are permutations of each other
let isPermutation a b =
    let aArray = a.ToString().ToCharArray() |> Array.sort
    let bArray = b.ToString().ToCharArray() |> Array.sort
    if Array.length aArray <> Array.length bArray then false
    else Array.forall2 (fun aChar bChar -> aChar = bChar) aArray bArray

// check semi-primes less than 10 million
let answer =
    primeNumbers
    |> Seq.collect (fun n ->
        primeNumbers 
        |> Seq.filter (fun n' -> n' > n) 
        |> Seq.map (fun n' -> n * n'))
        |> Seq.filter (fun n' -> n' > 8000000I && n' < 10000000I)
    |> Seq.map (fun n -> (n, totient n))
    |> Seq.filter (fun (n, n') -> isPermutation n n')
    |> Seq.minBy (fun (n, n') -> double(n) / double(n'))
