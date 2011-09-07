open System.Collections
 
let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
 
let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
 
let rec comb n l =
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs
 
let max = 9999
 
// define a cache for holding records of which number is a prime
let cache = new BitArray(max+1, true)
 
// using prime sieve to fill out the cache
[2..max]
    |> List.iter (fun n ->
        if cache.[n] then
            [2..max]
            |> Seq.takeWhile (fun m -> n * m <= max)
            |> Seq.iter (fun m -> cache.[n * m] <- false))
 
// build a list of prime numbers using the cache
let primeNumbers = [1000..max] |> List.filter (fun n -> cache.[n])
 
// define function to get the 4-digit prime permutations of a number
let getPrimePermutations n =
    let digitsStr = n.ToString().ToCharArray() |> Array.map string
    Array.toList digitsStr
    |> permute
    |> Seq.distinct
    |> Seq.map (fun chars -> int(chars |> List.reduce (+)))
    |> Seq.filter (fun x -> x >= 1000 && cache.[x])
    |> Seq.sort
    |> Seq.toList
 
let answer =
    primeNumbers
    |> List.map getPrimePermutations
    |> List.filter (fun l -> l |> List.length >= 3)
    |> Seq.distinct
    |> Seq.toList
    |> List.map (fun l -> comb 3 l |> List.filter (fun l' -> l'.[1] - l'.[0] = l'.[2] - l'.[1]))
    |> List.filter (fun l -> l |> List.length > 0)