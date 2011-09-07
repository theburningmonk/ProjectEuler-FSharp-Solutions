// the total and available coins
let total, coins = 200, [1;2;5;10;20;50;100;200]

// implement the coin change algorithm
let rec count n m =
    if n = 0 then 1
    else if n < 0 then 0
    else if (m <= 0 && n >= 1) then 0
    else (count n (m-1)) + (count (n-coins.[m-1]) m)

let answer = count total coins.Length