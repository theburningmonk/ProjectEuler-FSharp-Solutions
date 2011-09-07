let rec sumDiagonals n m total max =
    match n with
    | 1 -> sumDiagonals (n+1) m 1 1
    | _ when n > m -> total
    | _ when n % 2 = 0 -> sumDiagonals (n+1) m total max
    | _ ->
        let newValues = [1..4] |> List.map (fun x -> max + x * (n-1))
        let newMax = newValues |> List.max
        let newTotal = total + (newValues |> List.sum)
        sumDiagonals (n+1) m newTotal newMax
 
let answer = sumDiagonals 1 1001 0 0