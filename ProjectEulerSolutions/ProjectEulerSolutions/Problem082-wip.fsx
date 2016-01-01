open System
open System.IO

// load the original matrix
let matrix = 
    __SOURCE_DIRECTORY__ + "\Problem082_input.txt"
    |> File.ReadAllLines
    |> Array.map (fun l -> l.Split ',' |> Array.map int)
    |> array2D

let dim = Array2D.length1 matrix

// init the shortes sum matrix
let sumMatrix = 
    (fun row col -> 
        if col = 0 
        then matrix.[row, col] 
        else matrix.[row, 0..col] |> Array.sum)
    |> Array2D.init dim dim 

let fromTop row col =
    sumMatrix.[row-1, col] + matrix.[row, col]
let fromBelow row col =
    sumMatrix.[row+1, col] + matrix.[row, col]
let fromLeft row col =
    sumMatrix.[row, col-1] + matrix.[row, col]

let rec optimize col =
    let calNewMin row =
        if row = 0 then
            min (fromLeft row col) (fromBelow row col)
        elif row = dim-1 then
            min (fromLeft row col) (fromTop row col)
        else 
            fromLeft row col
            |> min (fromTop row col)
            |> min (fromBelow row col)

    let changes =
        { 0..dim-1 }
        |> Seq.sumBy (fun row ->
            let current = sumMatrix.[row, col]
            let newMin  = calNewMin row
            if newMin < current then
                sumMatrix.[row, col] <- newMin
                1
            else 0)

    if changes > 0 then optimize col

for col = 1 to dim-1 do
    optimize col

let answer = 
    [| 0..dim-1 |] 
    |> Array.map (fun row -> sumMatrix.[row, dim-1]) 
    |> Array.min