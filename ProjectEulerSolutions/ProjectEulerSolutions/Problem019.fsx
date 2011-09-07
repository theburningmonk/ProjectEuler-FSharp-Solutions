open System
 
let ans =
    [1901..2000]
    |> List.collect (fun y -> [1..12] |> List.map (fun m -> new DateTime(y, m, 1)))
    |> List.filter (fun d -> d.DayOfWeek = DayOfWeek.Sunday)
    |> List.length