let hasDivisor(n:bigint) =
    let upperBound = bigint(sqrt(double(n)))
    [2I..upperBound] |> Seq.exists (fun x -> n % x = 0I)
 
let isPrime(n:bigint) = if n = 1I then false else not(hasDivisor(n))
let primeSequence = Seq.unfold (fun state -> Some(state, (state+1I))) 1I |> Seq.filter isPrime
 
let rec recTruncatable (predicate:bigint -> bool) (next:bigint -> bigint) (n:bigint) =
    if predicate(n) then
        let len = n.ToString().Length
        if len = 1 then true else recTruncatable predicate next (next n)
    else false
 
let leftTruncatable = recTruncatable isPrime (fun x -> bigint.Parse(x.ToString().Substring(1)))
let rightTruncatable = recTruncatable isPrime (fun x -> bigint.Parse(x.ToString().Substring(0, x.ToString().Length-1)))
let sum =
    primeSequence
    |> Seq.filter (fun n -> n > 7I)
    |> Seq.filter (fun n -> leftTruncatable n && rightTruncatable n)
    |> Seq.take 11
    |> Seq.sum