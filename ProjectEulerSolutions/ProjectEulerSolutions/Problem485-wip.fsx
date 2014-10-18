#load "Common.fs"

open Common

#time

let primes = genPrimes 1000000 |> Set.ofArray

let d' n = 
    let rec loop n acc =
        if primes.Contains n then n::acc
        else 
            match primes |> Seq.tryFind (fun div -> n % div = 0) with
            | Some div -> loop (n / div) (div::acc)
            | _ -> n::acc

    let primeDivisors = loop n []
    primeDivisors 
    |> Seq.groupBy id 
    |> Seq.map (fun (_, gr) -> (gr |> Seq.length) + 1) 
    |> Seq.fold (*) 1
    |> int64

let d = memoize d'

let M'(n, k) = { n..n+k-1 } |> Seq.maxBy d
let M        = memoize M'
let S(u, k)  = { 1..u-k+1 } |> Seq.sumBy (fun n -> M(n, k) |> int64)

if 17176L <> S(1000, 10) then failwith "test case failed"
let answer = S(1000000000, 1000000)