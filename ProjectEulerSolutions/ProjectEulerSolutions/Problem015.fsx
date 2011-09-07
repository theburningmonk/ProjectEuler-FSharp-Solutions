let rec factorial(n:bigint) = if n <= 1I then 1I else n * factorial(n-1I)
let combo n k = factorial(n) / (factorial(k) * factorial(n-k))
let answer = combo 40I 20I