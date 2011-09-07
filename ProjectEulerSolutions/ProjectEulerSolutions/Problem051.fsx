// generate all prime numbers under <= this max
let max = 200000L

let mutable primeNumbers = [2L]

// only check the prime numbers which are <= the square root of the number n
let hasDivisor n =
    primeNumbers
    |> Seq.takeWhile (fun n' -> n' <= int64(sqrt(double(n))))
    |> Seq.exists (fun n' -> n % n' = 0L)

// only check odd numbers <= max
let potentialPrimes = Seq.unfold (fun n -> if n > max then None else Some(n, n+2L)) 3L

// populate the prime numbers list
for n in potentialPrimes do if not(hasDivisor n) then primeNumbers <- primeNumbers @ [n]

let isPrime n = if n = 1L then false else not(hasDivisor(n))

// define function to generate combinations of n elements out of the specified list
let rec comb n l =
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs

// define function to find the wild card digits of a n-digit number
let getWildCardDigits n = [1..n-1] |> List.collect (fun n' -> comb n' [0..n-1])

// define function to get the new numbers you get by replacing the digits in the
// supplied number n with the same digit
let replaceDigits (digits:int list) n =
    let nDigits = n.ToString().ToCharArray()
    [0..9] 
    |> List.map (fun n' -> 
        List.init nDigits.Length (fun d -> 
            if List.exists (fun d' -> d' = d) digits 
            then n'.ToString() 
            else nDigits.[d].ToString())
        |> List.reduce (+))
    |> List.map (int64)

// define function to find the lists (if any) of prime numbers obtained by replacing
// 1 or more digits of the supplied number n with the same digit
let F len n = 
    getWildCardDigits (n.ToString().Length)
    |> List.map (fun l -> 
        replaceDigits l n 
        |> List.filter (fun n' -> n'.ToString().Length = n.ToString().Length && isPrime n'))
    |> List.filter (fun l -> l.Length >= len)

let answer =
    primeNumbers
    |> Seq.skipWhile (fun n -> n < 56003L)
    |> Seq.map (F 8)
    |> Seq.filter (fun l -> l.Length > 0)
    |> Seq.head
    |> Seq.map (fun l -> List.min l)
    |> Seq.min