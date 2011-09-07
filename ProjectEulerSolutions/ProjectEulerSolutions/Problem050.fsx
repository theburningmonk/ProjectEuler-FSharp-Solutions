// generate all prime numbers under <= this max
let max = 10000
 
// initialise the list with 2 which is the only even number in the sequence
let mutable primeNumbers = [2]
 
// only check the prime numbers which are <= the square root of the number n
let hasDivisor n =
    primeNumbers
    |> Seq.takeWhile (fun n' -> n' <= int(sqrt(double(n))))
    |> Seq.exists (fun n' -> n % n' = 0)
 
// only check odd numbers <= max
let potentialPrimes = Seq.unfold (fun n -> if n > max then None else Some(n, n+2)) 3
 
// populate the prime numbers list
for n in potentialPrimes do
    if not(hasDivisor n) then primeNumbers <- primeNumbers @ [n]
 
// use the same hasDivisor function instead of the prime numbers list as it offers
// far greater coverage as the number n is square rooted so this function can
// provide a valid test up to max*max
let isPrime n = if n = 1 then false else not(hasDivisor(n))
 
// define function which computes the sum of the all consecutive primes starting
// from n, and returns the longest sequence which sums to a prime
let getPrimeSequence n =
    primeNumbers
    |> Seq.filter (fun n' -> n' > n)
    |> Seq.scan (fun (sum, count) n' -> (sum+n', count+1)) (n, 1)
    |> Seq.takeWhile (fun (sum, count) -> sum < 1000000)
    |> Seq.filter (fun (sum, count) -> isPrime sum)
    |> Seq.maxBy (fun (sum, count) -> count)
 
// for all numbers, find the longest sequence
let answer = primeNumbers |> Seq.map getPrimeSequence |> Seq.maxBy (fun (sum, count) -> count)