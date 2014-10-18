#load "Common.fs"

open Common

#time

let p3 n = n * (n + 1) /2
let p4 n = n * n
let p5 n = n * (3 * n - 1) / 2
let p6 n = n * (2 * n - 1)
let p7 n = n * (5 * n - 3) /2
let p8 n = n * (3 * n - 2)

let is4digit n = n >= 1000 && n <= 9999
let naturalNumbers = Seq.unfold (fun n -> Some (n+1, n+1)) 0

let candidates =
    [| 
        yield naturalNumbers |> Seq.map p3 |> Seq.skipWhile (not << is4digit) |> Seq.takeWhile is4digit |> Array.ofSeq
        yield naturalNumbers |> Seq.map p4 |> Seq.skipWhile (not << is4digit) |> Seq.takeWhile is4digit |> Array.ofSeq
        yield naturalNumbers |> Seq.map p5 |> Seq.skipWhile (not << is4digit) |> Seq.takeWhile is4digit |> Array.ofSeq
        yield naturalNumbers |> Seq.map p6 |> Seq.skipWhile (not << is4digit) |> Seq.takeWhile is4digit |> Array.ofSeq
        yield naturalNumbers |> Seq.map p7 |> Seq.skipWhile (not << is4digit) |> Seq.takeWhile is4digit |> Array.ofSeq
        yield naturalNumbers |> Seq.map p8 |> Seq.skipWhile (not << is4digit) |> Seq.takeWhile is4digit |> Array.ofSeq
    |]

let permutations = permute [0..5]

let processPermutation (indices : int list) =
    let rec loop pred (acc : int list) = function
        | [] when (Seq.head acc) % 100 = (Seq.last acc) / 100
             -> Some (acc |> List.rev)
        | [] -> None
        | n::rest ->
            candidates.[n] 
            |> Seq.skipWhile (not << pred)
            |> Seq.takeWhile pred
            |> Seq.tryPick (fun x ->
                let pred y = y / 100 = x % 100 // first 2 digit = last two digit of x
                loop pred (x::acc) rest)
    loop (fun _ -> true) [] indices

let answer = permutations 
             |> List.tryPick processPermutation
             |> Option.get
             |> List.sum