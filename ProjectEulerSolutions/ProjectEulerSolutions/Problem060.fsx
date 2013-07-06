#load "Common.fs"

open Common

let max = 10000
let primes = genPrimes max |> Array.toList

let isPrime' n = 
    if n % 2 = 0 then false 
    elif n % 3 = 0 then false
    else let sqrtn = float n |> sqrt |> ceil |> int
         seq { 5..2..sqrtn } |> Seq.exists (fun x -> n % x = 0) |> not
let isPrime = memoize isPrime'

let concatsToPrime' (a, b) = 
    let f a b = a * pown 10 (int (log10 (float b) + 1.0)) + b
    isPrime(f a b) && isPrime(f b a)
let concatsToPrime = memoize concatsToPrime'

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

let combos = comb 5 primes [] |> Seq.map List.sum |> Seq.min
