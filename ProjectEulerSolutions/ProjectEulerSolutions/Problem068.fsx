#load "Common.fs"

open Common

#time

let permutations = permute [1..10] |> List.map (fun lst -> groupsOfAtMost 2 lst |> Seq.toList)

let sum [| [| a0; a1 |]; [| _; b1 |] |] = a0 + a1 + b1       

let pred (permutation : int[] list) =
    let (hd::_tl as heads) = permutation |> List.map Seq.head
    if heads |> List.forall (fun x -> hd <= x) then
        let pairs  = seq {
                        yield! permutation |> Seq.windowed 2
                        yield  [| Seq.last permutation; Seq.head permutation |]
                     } |> Seq.toArray
        let target = sum pairs.[0]
        pairs |> Array.forall (sum >> (=) target)
    else false
    
let answer = permutations 
             |> List.filter pred
             |> List.map (fun [a; b; c; d; e] -> 
                 [|
                     yield! a; yield Seq.last b
                     yield! b; yield Seq.last c
                     yield! c; yield Seq.last d
                     yield! d; yield Seq.last e
                     yield! e; yield Seq.last a
                 |]
                 |> Array.map string
                 |> fun arr -> System.String.Join("", arr))
             |> List.sort
             |> Seq.last