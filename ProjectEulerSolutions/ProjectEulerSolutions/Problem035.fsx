open System
 
let hasDivisor(n:bigint) =
    let upperBound = bigint(sqrt(double(n)))
    [2I..upperBound] |> Seq.exists (fun x -> n % x = 0I)
 
let isPrime(n:bigint) = if n = 1I then false else not(hasDivisor(n))
 
let rotate(n:bigint) =
    let charList =n.ToString().ToCharArray() |> Array.toList
    let len = List.length charList
    [0..(len-1)]
    |> List.map (fun r -> List.permute (fun i -> (i + r) % len) charList)
    |> List.map (fun l -> String.Join("", l |> List.toArray))
    |> List.map bigint.Parse
 
let isCircularPrime(n:bigint) = rotate n |> List.forall isPrime
 
let answer = [2I..999999I] |> Seq.filter isPrime |> Seq.filter isCircularPrime |> Seq.length