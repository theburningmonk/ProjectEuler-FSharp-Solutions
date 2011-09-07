open System
open System.Linq
 
// checks if the number n is palindromic in the supplied base b
let isPalindromic (b:int) (n:int) =
    let charArray = Convert.ToString(n, b).ToCharArray()
    let revCharArray = Array.rev charArray
    charArray.SequenceEqual(revCharArray)
 
// using function currying to build two higher-order functions to check
// if number is palindormic in base 10 and base 2 separately
let isPalindromicBase10 = isPalindromic 10
let isPalindromicBase2 = isPalindromic 2
 
let answer =
    [1..1000000]
    |> List.filter (fun n -> isPalindromicBase10 n && isPalindromicBase2 n)
    |> List.sum