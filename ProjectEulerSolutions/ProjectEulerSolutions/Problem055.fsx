open System.Linq
 
// checks if the number n is palindromic
let isPalindromic n =
    let charArray = n.ToString().ToCharArray()
    let revCharArray = Array.rev charArray
    charArray.SequenceEqual(revCharArray)
 
// reverse a number, e.g. 1234 -> 4321
let reverse n = 
    bigint.Parse(n.ToString().ToCharArray() |> Array.rev |> Array.map string |> Array.reduce (+))
 
// checks if a number is lychrel, i.e. not produce palindromic sum in 50 iterations
let rec isLychrelRec i (number:bigint) =
    let sum = number + (reverse number)
    if isPalindromic sum then false
    else if i >= 49 then true else isLychrelRec (i+1) sum
 
// curry the recursive isLychrelRec function and initialize with 1
let isLychrel = isLychrelRec 1
 
let answer = [1I..10000I] |> List.filter isLychrel |> List.length