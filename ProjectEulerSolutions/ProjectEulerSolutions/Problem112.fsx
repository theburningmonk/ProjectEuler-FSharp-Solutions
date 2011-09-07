// define function f which compares the sorted and original digits of the supplied
// number n and returns if they're the same
let f (sort:int[] -> int[]) (n:int) = 
    let digits = n.ToString().ToCharArray() |> Array.map (fun c -> int(c.ToString()))
    not(digits |> Seq.exists2 (fun e e' -> e <> e') (sort digits))

// curry the function f to form the isIncreasing and isDecreasing functions
let isIncreasing = f (fun l -> Array.sort l)
let isDecreasing = f (fun l -> Array.sortBy (fun x -> 1 - x) l)

let isBouncy n = not(isIncreasing n) && not(isDecreasing n)

let answer =
    let mutable bouncyCount, bouncyRatio, n = 0, 0.0, 100

    while bouncyRatio <> 0.99 do
        n <- n+1
        if isBouncy n then
            bouncyCount <- bouncyCount+1
            bouncyRatio <- double(bouncyCount)/double(n)

    n