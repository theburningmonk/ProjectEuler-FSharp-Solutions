open System.Collections.Generic
 
// factorial function
let factorial n = if (n = 0) then 1 else [1..n] |> List.reduce (fun acc x -> acc * x)
 
// create a cache of sorts to help improve performance
let factorials = new Dictionary<int, int>()
 
let start = [0..9] |> List.iter (fun n -> factorials.Add(n, (factorial n)))
 
// get the digits of a number into an array
let getDigits (n:bigint) =
    n.ToString().ToCharArray()
    |> Array.map (fun c -> int(c.ToString()))
 
// get the sum of the factorials of a number's digits
let getDigitsToFactSum (n:bigint) =
    bigint(getDigits(n) |> Array.map (fun n -> factorials.[n]) |> Array.sum)
 
// get the max sum achievable by a number of the given number of digits
let upperBound (digits:int) = bigint(digits) * bigint(factorial 9)
 
// find the last number of digits n, where the max sum achievable by the factorials of the
// digits of a number of n digits is greater than the smallest number of n digits
// any number with more than n digits do not need to be checked
let digitsToCheck =
    let n =
        Seq.unfold (fun state -> Some(state, (state+1))) 1
        |> Seq.filter (fun x -> (upperBound x).ToString().ToCharArray().Length < x)
        |> Seq.head
    n-1
 
// get the next number with the given number of digits
let maxNumber digits = [1..digits] |> List.map (fun x -> 9I * pown 10I (x-1)) |> List.sum
 
let answer =
    let max = maxNumber digitsToCheck
    [3I..max] |> List.filter (fun n -> n = getDigitsToFactSum n) |> List.sum