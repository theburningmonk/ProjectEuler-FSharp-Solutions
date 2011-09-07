// define function to check if two numbers has the cancelling behaviour
let isCancelling a b =
    let aStr, bStr = a.ToString(), b.ToString()
    if aStr.[0] = bStr.[0] then double(aStr.[1].ToString())/double(bStr.[1].ToString()) = double(a)/double(b)
    else if aStr.[0] = bStr.[1] then double(aStr.[1].ToString())/double(bStr.[0].ToString()) = double(a)/double(b)
    else if aStr.[1] = bStr.[0] then double(aStr.[0].ToString())/double(bStr.[1].ToString()) = double(a)/double(b)
    else if aStr.[1] = bStr.[1] then double(aStr.[0].ToString())/double(bStr.[0].ToString()) = double(a)/double(b)
    else false
 
// define function to find the greatest common dividor
let gcd a b = [2..min a b] |> List.rev |> Seq.filter (fun x -> a % x = 0 && b % x = 0) |> Seq.head
 
// define function that returns numbers >= n which are not multiples of 10
// and have two distinct digits (i.e. 11, 22 only have one distinct digits)
let numbers n = [n..99] |> List.filter (fun x -> x % 10 <> 0 && (x % 10 <> x/10))
 
let answer =
    // first work out the product of all 4 fractions, in fractions form (num, denom)
    let fraction =
        numbers 10
        |> List.collect (fun x -> numbers (x+1) |> List.map (fun y -> (x, y)))
        |> List.filter (fun (x, y) -> isCancelling x y)
        |> List.reduce (fun (num, denom) (x, y) -> (num*x, denom*y))
    // then define the denominator by the greatest common divisor
    (snd fraction) / gcd (fst fraction) (snd fraction)