// implement the coin change algorithm
let rec count n m (coins:int list) =
    if n = 0 then 1
    else if n < 0 then 0
    else if (m <= 0 && n >= 1) then 0
    else (count n (m-1) coins) + (count (n-coins.[m-1]) m coins)

let answer = count 100 99 [1..99]