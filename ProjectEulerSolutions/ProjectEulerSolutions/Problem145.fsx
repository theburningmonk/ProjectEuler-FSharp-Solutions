let digitBases = [9..-1..0] |> List.map (fun n -> pown 10 n)

/// turns the specified number into a list of its digits
let toDigits (n : int) = 
    digitBases |> List.filter (fun n' -> n > n') |> List.map (fun n' -> (n / n') % 10)

/// turns the specified list of digits into the number they represent
let toNumber (digits : int list) =
    digits |> List.mapi (fun i n -> n * pown 10 (digits.Length - i - 1)) |> List.sum

/// returns true if the specified number is reversible, otherwise returns false
let isReversible (n :int) = 
    let digits = toDigits n
    let revDigits = digits |> List.rev

    if (revDigits.Head = 0) then false
    else
        let nRev = toNumber revDigits
        let sum = n + nRev
        let sumDigits = toDigits sum
        sumDigits |> List.forall (fun n' -> n' % 2 <> 0)

// only check odd numbers, as a pair of reversible numbers will consist off one odd
// and one even number otherwise their sum will always be even
let answer = (seq { 3..2..1000000000 } |> Seq.filter isReversible |> Seq.length) * 2