// get the digits of a number into an array
let getDigits (n:bigint) = n.ToString().ToCharArray() |> Array.map (fun c -> bigint.Parse(c.ToString()))
 
// get the sum of a number's digits to the specified power
let getDigitsToPowerSum (n:bigint) pow = 
    getDigits(n) |> Array.map (fun x -> pown x pow) |> Array.sum
 
// get the max sum achievable by a number of the given number of digits to the specified power
let upperBound (digits:int) pow = bigint(digits) * pown 9I pow
 
// find the last number of digits n, where the max sum achievable by the digits of a number of
// n digits to the specified power is greater than the smallest number of n digits
// any number with more than n digits do not need to be checked
let digitsToCheck pow =
    let n =
        Seq.unfold (fun state -> Some(state, (state+1))) 1
        |> Seq.filter (fun x -> (upperBound x pow).ToString().ToCharArray().Length < x)
        |> Seq.head
    n-1
 
// get the next number with the given number of digits
let maxNumber digits = [1..digits] |> List.map (fun x -> 9I * pown 10I (x-1)) |> List.sum
 
let answer =
    let max = maxNumber (digitsToCheck 5)
    [2I..max] |> List.filter (fun n -> n = getDigitsToPowerSum n 5) |> List.sum