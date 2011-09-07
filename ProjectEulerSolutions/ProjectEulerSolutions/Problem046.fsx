let hasDivisor(n) =
    let upperBound = int64(sqrt(double(n)))
    [2L..upperBound] |> Seq.exists (fun x -> n % x = 0L)
 
// need to consider negative values
let isPrime(n) = if n <= 1L then false else not(hasDivisor(n))
 
// generate the sequence of odd composite numbers
let oddCompositeNumbers = 
    Seq.unfold (fun state -> Some(state, state+2L)) 9L 
    |> Seq.filter (fun n -> not(isPrime n))
 
// generate the sequence of prime numbers
let primeNumbers = Seq.unfold (fun state -> Some(state, state+2L)) 1L |> Seq.filter isPrime
 
// function to check if a number can be written as the sum of a prime and twice a square
let isSum(number) =
    primeNumbers
    |> Seq.takeWhile (fun n -> n < number)
    |> Seq.exists (fun n -> sqrt(double((number-n)/2L)) % 1.0 = 0.0)
 
let answer = oddCompositeNumbers |> Seq.filter (fun n -> not(isSum(n))) |> Seq.head