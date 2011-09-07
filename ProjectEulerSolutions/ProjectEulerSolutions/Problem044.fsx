open System.Collections.Generic
 
// define the Pentagonal function P
let P n = n*(3*n-1)/2
 
// use a dictionary as cache
let mutable cache = new Dictionary<int, int>()
[1..5000] |> List.iter (fun n -> cache.[n] <- P n)
 
// function to check if a number is pentagonal by looking at the cached values
let isPentagonal n = cache.ContainsValue n
 
// predicate function to check if Pk and Pj's sum and diff are both pentagonal
let predicate (k, j) =
    let pk = cache.[k]
    let pj = cache.[j]
    isPentagonal (pj + pk) && isPentagonal (pk - pj)
 
// the sequence of k, j pairs to check
let kjSeq =
    [1..5000]
    |> List.collect (fun k -> [1..k-1] |> List.rev |> List.map (fun j -> (k, j)))
 
// get the first pair of k, j whose sum and difference are both pentagonal
let (k, j) = kjSeq |> Seq.filter predicate |> Seq.head
 
let answer = (P k) - (P j)