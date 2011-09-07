open System
 
let triangleNumber(n:int64) = [1L..n] |> Seq.sum
 
let findFactorsOf(n:int64) =
    let upperBound = int64(Math.Sqrt(double(n)))
    [1L..upperBound] 
    |> Seq.filter (fun x -> n % x = 0L) 
    |> Seq.collect (fun x -> [x; n/x])
 
let naturalNumbers = Seq.unfold (fun x -> Some(x, x+1L)) 1L
 
let answer =
    naturalNumbers
    |> Seq.map (fun x -> triangleNumber(x))
    |> Seq.filter (fun x -> Seq.length(findFactorsOf(x)) >= 500)
    |> Seq.head