open System.Numerics
 
// for each d, get the number of proper fractions that lie between 1/3 and 1/2
let getFractionsCount d =
    [1I..d-1I]
    |> Seq.skipWhile (fun n -> n * 3I <= d)
    |> Seq.takeWhile (fun n -> n * 2I < d)
    |> Seq.filter (fun n -> BigInteger.GreatestCommonDivisor(n, d) = 1I)
    |> Seq.length
 
// ignoring 1-4, get the number of desired fractions for each dand add them up
let answer = [4I..12000I] |> List.map getFractionsCount |> List.sum