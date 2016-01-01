#load "Common.fs"

open Common
open Checked

#time

let sqrt   = sqrt >> int
let primes = genPrimes (sqrt 100000000.0) |> Set.ofArray

let pred n =
    let max = sqrt (float n)

    { 1..max }
    |> Seq.filter (fun m -> n % m = 0)
    |> Seq.forall (fun m -> primes.Contains <| m + n/m)
   
let answer = { 1..100000000 } |> Seq.filter pred |> Seq.sum