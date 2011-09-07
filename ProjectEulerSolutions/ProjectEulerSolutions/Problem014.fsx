let nextNumber n = if n%2L = 0L then n/2L else 3L*n+1L
 
let findSequenceLength n =
    let mutable count = 1L
    let mutable current = n
 
    while current > 1L do
        current <- nextNumber current
        count <- count + 1L
    count
 
let longestSeq = [1L..999999L] |> Seq.maxBy findSequenceLength