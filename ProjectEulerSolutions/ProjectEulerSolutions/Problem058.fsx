// generate all prime numbers under <= this max
let max = 100000
 
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
 
// define function that returns the number on the corners of a spiral of length n
let getCornerNumbers n =
    match n with
    | 1 -> [1]
    | _ when n % 2 = 0 -> []
    | _ -> [3..-1..0] |> List.map (fun n' -> n*n - n'*(n-1))
 
let answer =
    let mutable cornerNumbers, primeNumbers, size = 0, 0, 1
    let mutable continueLoop = true
 
    while continueLoop do
        // get the numbers that appear at the corners of a spiral of the given size
        let newNumbers = getCornerNumbers size
 
        // increment the totals
        cornerNumbers <- cornerNumbers + newNumbers.Length
        primeNumbers <- primeNumbers + (newNumbers |> List.filter isPrime |> List.length)
 
        let ratio = double(primeNumbers) / double(cornerNumbers)
        if ratio < 0.1 && size > 1 then continueLoop <- false else size <- size + 2
    size