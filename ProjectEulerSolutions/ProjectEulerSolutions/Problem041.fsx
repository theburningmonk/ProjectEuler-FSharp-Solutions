let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
 
let hasDivisor(n) =
    let upperBound = int64(sqrt(double(n)))
    [2L..upperBound] |> Seq.exists (fun x -> n % x = 0L)
let isPrime(n) = if n = 1L then false else not(hasDivisor(n))
 
let answer =
    [1..9]
    |> List.collect (fun m -> [1..m] |> permute)
    |> List.map (fun l -> l |> List.map string |> List.reduce (fun acc item -> acc + item))
    |> List.map int64
    |> List.filter isPrime
    |> List.max