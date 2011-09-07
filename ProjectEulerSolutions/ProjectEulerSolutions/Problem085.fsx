open System

let target = 2000000

// function to work out the number of rects in a grid of x by y
let getRectCount x y = (x * x + x) * (y * y + y) / 4

// try x and y dimensions up to 100
let answer = 
    seq {
        for x in 2..100 do
            for y in 2..100 do
                // get the grid area and the difference to the target count in a tuple
                yield (x * y, Math.Abs(getRectCount x y - target))
    }
    |> Seq.minBy (fun (area, diff) -> diff)