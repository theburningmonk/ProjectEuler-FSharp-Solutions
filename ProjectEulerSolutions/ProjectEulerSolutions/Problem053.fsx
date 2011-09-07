let factorial n = if (n = 0I) then 1I else [1I..n] |> List.reduce (*)
 
let C n r = if r <= n then (factorial n) / ((factorial r) * (factorial (n - r))) else 0I
 
let answer =
    [1I..100I]
    |> List.collect (fun n -> [1I..n] |> List.map (fun r -> C n r))
    |> List.filter (fun x -> x > 1000000I)
    |> List.length