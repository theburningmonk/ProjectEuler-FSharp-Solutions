#load "Common.fs"

open Common

// test primes up to a max value
let max = 10000
let primes = genPrimes max |> Array.toList

// the prime numbers we generated is not going to be sufficient to cover the
// concatenated primes, hence the isPrime' function here and its memoized form
let isPrime' n = 
    if n % 2 = 0 then false 
    elif n % 3 = 0 then false
    else let sqrtn = float n |> sqrt |> ceil |> int
         seq { 5..2..sqrtn } |> Seq.exists (fun x -> n % x = 0) |> not
let isPrime = memoize isPrime'

// concatenates two numbers together in both ways and check if both concatenated
// numbers are also primes
let concatsToPrime' (a, b) = 
    let f a b = a * pown 10 (int (log10 (float b) + 1.0)) + b
    isPrime(f a b) && isPrime(f b a)
let concatsToPrime = memoize concatsToPrime'

// find all combinations of n elements in the list l where every two element
// concatenates to a prime
let rec comb n (l : int list) acc =
    match n, l with
    | 0, _  -> seq { yield [] }
    | _, [] -> Seq.empty<int list>
    | k, (x::xs) -> 
        seq {
            if acc |> List.forall (fun y -> concatsToPrime(x, y)) 
            then yield! Seq.map ((@) [x]) (comb (k-1) xs (x::acc))

            yield! comb k xs acc
        }

let answer = comb 5 primes [] |> Seq.map List.sum |> Seq.min