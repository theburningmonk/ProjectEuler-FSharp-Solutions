open System
 
let findDivisors(n) =
    let upperBound = int32(Math.Sqrt(double(n)))
 
    [1..upperBound]
    |> Seq.filter (fun x -> n % x = 0)
    |> Seq.collect (fun x -> [x; n/x])
    |> Seq.filter (fun x -> x <> n)
 
let d(n) = findDivisors(n) |> Seq.sum
let dList = [ for n = 1 to 9999 do yield (n, d(n)) ]
 
let answer =
    dList
    |> List.filter (fun (a, da) -> dList
                                   |> List.exists (fun (b, db) -> b = da && a = db && a <> b))
    |> List.sumBy (fun (n, dn) -> n)