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

// returns the set of primes that concatenates with n to another prime
let getConcatenbleSet' n = 
    primes |> List.filter ((fun n' -> n' > n) <&&> (fun n' -> concatsToPrime(n, n'))) |> Set.ofList
let getConcatenbleSet = memoize getConcatenbleSet'

let rec comb n l (s : Set<int>) =
    match n, l with
    | 0, _  -> seq { yield [] }
    | _, [] -> Seq.empty<int list>
    | 1, [x] -> seq { yield [x] }
    | k, x::xs ->
        seq {
            // find the set of numbers th
            let s'  = getConcatenbleSet x |> Set.intersect s
            if not s'.IsEmpty then
                yield! Seq.map ((@) [x]) (comb (k-1) (s' |> Set.toList) s')

            yield! comb k xs s
        }

let answer = comb 5 primes (primes |> Set.ofList) |> Seq.map List.sum |> Seq.min