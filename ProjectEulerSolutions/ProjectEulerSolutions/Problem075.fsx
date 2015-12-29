#load "Common.fs"

open Common

#time

// see Pythagorean Triples on wikipedia
//  https://en.wikipedia.org/wiki/Pythagorean_triple

let maxL = 1500000
let maxM = sqrt(float maxL / 2.0) |> int

let ``m & n`` =
    { maxM .. -1 .. 2 }
    |> Seq.collect (fun m ->
        { m-1 .. -1 .. 1 }
        |> Seq.filter (fun n -> (m - n) % 2 = 1 && gcd m n = 1)
        |> Seq.map (fun n -> m, n))

let cache = Array.zeroCreate<byte>(maxL+1)
``m & n``
|> Seq.iter (fun (m, n) ->
    let a = m*m - n*n
    let b = 2*m*n
    let c = m*m + n*n
    let L = a + b + c
    { 1..maxL/L }
    |> Seq.iter (fun x ->
        cache.[x*L] <- cache.[x*L] + 1uy))

let answer = cache |> Array.filter ((=) 1uy) |> Array.length