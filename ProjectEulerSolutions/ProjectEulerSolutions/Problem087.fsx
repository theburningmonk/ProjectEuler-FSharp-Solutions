// generate all prime numbers under <= this max
let max = int64(sqrt(double(50000000L)))

// initialise the list with 2 which is the only even number in the sequence
let mutable primeNumbers = [2L]

// only check the prime numbers which are <= the square root of the number n
let hasDivisor n =
    primeNumbers
    |> Seq.takeWhile (fun n' -> n' <= int64(sqrt(double(n))))
    |> Seq.exists (fun n' -> n % n' = 0L)

// only check odd numbers <= max
let potentialPrimes = Seq.unfold (fun n -> if n > max then None else Some(n, n+2L)) 3L

// populate the prime numbers list
for n in potentialPrimes do
    if not(hasDivisor n) then primeNumbers <- primeNumbers @ [n]

// use the same hasDivisor function instead of the prime numbers list as it offers
// far greater coverage as the number n is square rooted so this function can
// provide a valid test up to max*max
let isPrime n = if n = 1L then false else not(hasDivisor(n))

let answer =
    primeNumbers
    |> Seq.collect (fun n -> 
        primeNumbers 
        |> Seq.map (fun n' -> pown n 2 + pown n' 3)
        |> Seq.takeWhile (fun sum -> sum < 50000000L))
    |> Seq.collect (fun sum ->
        primeNumbers
        |> Seq.map (fun n -> sum + pown n 4)
        |> Seq.takeWhile (fun sum' -> sum' < 50000000L))
    |> Seq.distinct
    |> Seq.length