// define a function which returns the last n digits of a number
let getLastDigitsOf n number =
    let numberStr = number.ToString()
    let digits =
        if numberStr.Length > n then numberStr.Substring(numberStr.Length - n, n)
        else numberStr.Substring(0, numberStr.Length)
    int64(digits)
 
// define a function which iteratively powers up the base (b) but all the time
// only keeping track of the last n digits of the result
let F b pow n =
    let mutable tracker = 1L
    for i = 1 to pow do tracker <- getLastDigitsOf n (tracker * b)
    tracker
 
let answer = getLastDigitsOf 10 (28433L * (F 2L 7830457 10) + 1L)