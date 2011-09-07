let hasDivisor(n) =
    let upperBound = int64(sqrt(double(n)))
    [2L..upperBound] |> Seq.exists (fun x -> n % x = 0L)
 
// need to consider negative values
let isPrime(n) = if n <= 1L then false else not(hasDivisor(n))
 
// the quadratic expression
let F (n:int64) (a:int64) (b:int64) = (n*n) + (a*n) + b
 
// function to return the number of consecutive primes the coefficients generate
let primeCount a b =
    Seq.unfold (fun state -> Some(state, state + 1L)) 0L
    |> Seq.takeWhile (fun n -> isPrime (F n a b))
    |> Seq.length
 
let aList, bList = [-999L..999L], [2L..999L] |> List.filter isPrime
 
let answer =
    let (a, b, _) =
        aList
        |> List.collect (fun a ->
            bList
            |> List.filter (fun b -> a + b >= 1L)
            |> List.map (fun b -> (a, b, primeCount a b)))
        |> List.maxBy (fun (_, _, count) -> count)
    a * b