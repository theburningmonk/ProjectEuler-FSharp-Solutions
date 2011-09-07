let findDivisors(n) =
   let upperBound = int32(sqrt(double(n)))
 
   [1..upperBound]
   |> Seq.filter (fun x -> n % x = 0)
   |> Seq.collect (fun x -> [x; n/x])
   |> Seq.filter (fun x -> x <> n)
   |> Seq.distinct
 
let isAbundantNumber(n) = (findDivisors(n) |> Seq.sum) > n
 
let abundantNumbers =
   Seq.unfold (fun x -> if x < 28123 then Some(x, x+1) else None) 1
   |> Seq.filter isAbundantNumber
   |> Seq.toList
 
let abundantNumbersSums =
   abundantNumbers
   |> Seq.collect (fun n -> abundantNumbers |> List.map (fun m -> n + m))
   |> Seq.filter (fun n -> n <= 28123)
   |> Seq.distinct
   |> Seq.toList
 
let answer = ([1..28123] |> List.sum) - (abundantNumbersSums |> List.sum)