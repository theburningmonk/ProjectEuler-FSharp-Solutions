let number = 2I ** 1000
let answer = number.ToString() |> Seq.map (fun c -> int32(c.ToString())) |> Seq.sum