#load "Common.fs"

open Common

let isPrime' n = 
    if n % 2L = 0L then false 
    elif n % 3L = 0L then false
    else let sqrtn = float n |> sqrt |> ceil |> int64
         seq { 5L..2L..sqrtn } |> Seq.exists (fun x -> n % x = 0L) |> not
let isPrime = memoize isPrime'

let M n d =
    let gen () =
        