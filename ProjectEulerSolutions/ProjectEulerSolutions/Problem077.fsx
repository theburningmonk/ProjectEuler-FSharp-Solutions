// generate all prime numbers under <= this max
let max = 1000
let mutable primeNumbers = [2]

// only check the prime numbers which are <= the square root of the number n
let hasDivisor n =
    primeNumbers
    |> Seq.takeWhile (fun n' -> n' <= int(sqrt(double(n))))
    |> Seq.exists (fun n' -> n % n' = 0)

// only check odd numbers <= max
let potentialPrimes = Seq.unfold (fun n -> if n > max then None else Some(n, n+2)) 3

// populate the prime numbers list
for n in potentialPrimes do if not(hasDivisor n) then primeNumbers <- primeNumbers @ [n]
let isPrime n = if n = 1 then false else not(hasDivisor(n))

// implement the coin change algorithm
let rec count n m (coins:int list) =
    if n = 0 then 1
    else if n < 0 then 0
    else if (m <= 0 && n >= 1) then 0
    else (count n (m-1) coins) + (count (n-coins.[m-1]) m coins)

let answer = 
    let tuple = 
        Seq.unfold (fun state -> Some(state, state+1)) 10
        |> Seq.map (fun n -> (n, primeNumbers |> Seq.filter (fun n' -> n' < n) |> Seq.cache))
        |> Seq.filter (fun (n, l) -> count n (Seq.length l) (Seq.toList l) > 5000)
        |> Seq.head
    fst tuple