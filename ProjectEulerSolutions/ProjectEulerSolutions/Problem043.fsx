let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
 
let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)
 
// generate the 0 to 9 pandigitals
let numbers = permute [0..9] |> List.map (fun l -> l |> List.map string |> List.reduce (+))
 
// the corresponding prime divisors and digits
let primes = [2; 3; 5; 7; 11; 13; 17]
let ns = [2..10] |> Seq.windowed 3 |> Seq.toList
 
// returns the number retrieved from taking the digits at the supplied positions
let d ns (numberStr:string) = 
    int(ns |> Array.map (fun n -> numberStr.[n-1].ToString()) |> Array.reduce (+))
 
// predicate which identifies pandigital numbers with the desired property
let predicate number = List.forall2 (fun n p -> (d n number) % p = 0) ns primes
 
let answer = numbers |> List.filter predicate |> List.sumBy int64