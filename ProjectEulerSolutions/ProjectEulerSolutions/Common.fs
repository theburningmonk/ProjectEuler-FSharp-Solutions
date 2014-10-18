module Common

open System.Collections
open System.Collections.Generic

let (<&&>) f g x = f x && g x
let (<||>) f g x = f x || g x

/// Applies memoization to the supplied function f
let memoize (f : 'a -> 'b) =
    let cache = new Dictionary<'a, 'b>()

    let memoizedFunc (input : 'a) =
        // check if there is a cached result for this input
        match cache.TryGetValue(input) with
        | true, x   -> x
        | false, _  ->
            // evaluate and add result to cache
            let result = f input
            cache.Add(input, result)
            result

    // return the memoized version of f
    memoizedFunc

// generate prime numbers up to the specified max
let genPrimes max =
    // define a cache for holding records of which number is a prime
    let cache = new BitArray(max+1, true)
 
    // using prime sieve to fill out the cache
    [| 2..max |]
    |> Array.iter (fun n ->
        if cache.[n] then
            [| 2..max / n |] 
            |> Array.iter (fun m -> cache.[n * m] <- false))
 
    // build a set of prime numbers using the cache
    [| 2..max |] 
    |> Array.filter (fun n -> cache.[n])

let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
 
// generate permutations of a list
let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
 
// generate combinations of n elements from the list l
let rec comb n (l : 'a list) =
    match n, l with
    | 0, _ -> seq { yield [] }
    | _, [] -> Seq.empty<'a list>
    | k, (x::xs) -> 
        seq {
            yield! Seq.map ((@) [x]) (comb (k-1) xs)
            yield! comb k xs
        }