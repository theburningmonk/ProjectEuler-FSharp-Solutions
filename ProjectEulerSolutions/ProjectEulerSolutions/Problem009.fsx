let isPythagoreanTriplet(numbers : int list) =
    match List.sort(numbers) with
    | [a; b; c] -> a*a + b*b = c*c
    | _ -> false
 
let getTriplets =
    seq {
        for a = 1 to 1000 do
            for b = 1 to 1000 do
                for c = 1 to 1000 do
                    if a + b + c = 1000 then yield [a; b; c]
    }
 
let pythagoreanTriplet = getTriplets |> Seq.filter isPythagoreanTriplet |> Seq.head
let product = pythagoreanTriplet |> Seq.fold (fun acc x -> acc * x) 1