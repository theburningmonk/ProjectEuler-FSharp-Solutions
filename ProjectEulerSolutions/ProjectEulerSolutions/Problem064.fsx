#time

let expand n =
    let m0, d0, a0 = 0, 1, float n |> sqrt |> int

    let (|Repeat|_|) (acc : int list) =
        if acc.Length = 0 || acc.Length % 2 <> 0 then None
        elif Seq.head acc <> 2 * a0 then None
        else let half = acc.Length / 2
             let fst, snd = acc |> Seq.take half |> Seq.toArray, 
                            acc |> Seq.skip half |> Seq.toArray
             if fst = snd then Some fst else None

    let rec loop m d a = function
        | Repeat arr -> arr.Length
        | acc ->
            let m' = d * a - m
            let d' = (n - (m' * m')) / d
            let a' = (a0 + m') / d' |> int
            loop m' d' a' (a'::acc)
    loop m0 d0 a0 []

let perfectSqrs = [| 1..100 |] |> Array.map (fun x -> x * x) |> Set.ofArray
let nums        = Set.difference (Set [| 1..10000 |]) perfectSqrs
let answer = nums |> Seq.filter (fun x -> expand x % 2 <> 0) |> Seq.length