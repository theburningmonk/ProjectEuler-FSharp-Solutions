// the pentagonal numbers sequence, i.e. 1, 2, 5, 7, 12, 15, 22, ...
let pentagonalNumbers = 
    Seq.unfold (fun state -> Some(state, state+1)) 1
    |> Seq.collect (fun n -> [n; -1*n])
    |> Seq.map (fun n -> int(0.5 * double(n) * double(3 * n - 1)))

// the coefficients sequence, i.e. +, +, -, -, +, +, -, -, ...
let pCoefficients = 
    Seq.unfold (fun state -> Some(state, -1*state)) 1 |> Seq.collect (fun n -> [n; n])

// cache results to improve performance
let mutable cache = Array.init 100000 (fun n -> if n = 0 then 1I else 0I)

// define the function p using the pentagonal numbers
let rec p k =
    if cache.[k] <> 0I then cache.[k]
    else
        let pSeq = 
            pentagonalNumbers 
            |> Seq.map (fun n -> k - n) 
            |> Seq.takeWhile (fun n -> n >= 0)
            |> Seq.map p
        let pk =
            pCoefficients 
            |> Seq.zip pSeq
            |> Seq.sumBy (fun (pk, coe) -> pk * bigint(coe))
        cache.[k] <- pk
        pk

let answer = 
    Seq.unfold (fun state -> Some(state, state+1)) 1
    |> Seq.filter (fun k -> (p k) % 1000000I = 0I)
    |> Seq.head