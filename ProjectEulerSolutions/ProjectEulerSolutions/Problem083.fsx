open System.IO

#time

// load the original matrix
let matrix = 
    __SOURCE_DIRECTORY__ + "\Problem083_input.txt"
    |> File.ReadAllLines
    |> Array.map (fun l -> l.Split ',' |> Array.map int)
    |> array2D

let dim = Array2D.length1 matrix

// init the shortes sum matrix
let sumMatrix = 
    (fun row col -> 
        match row, col with
        | 0, 0 -> matrix.[row, col] 
        | 0, _ -> matrix.[row, 0..col] |> Array.sum
        | _, _ -> 
            let hSum = matrix.[0, 0..col] |> Array.sum
            let vSum = matrix.[1..row, col] |> Array.sum
            hSum + vSum)
    |> Array2D.init dim dim 

let fromTop row col =
    sumMatrix.[row-1, col] + matrix.[row, col]
let fromBelow row col =
    sumMatrix.[row+1, col] + matrix.[row, col]
let fromLeft row col =
    sumMatrix.[row, col-1] + matrix.[row, col]
let fromRight row col =
    sumMatrix.[row, col+1] + matrix.[row, col]

let rec optimize () =
    let calNewMin row col =
        seq {
            if row > 0 then yield fromTop row col
            if row < dim-1 then yield fromBelow row col
            if col > 0 then yield fromLeft row col
            if col < dim-1 then yield fromRight row col
        }
        |> Seq.min

    let cells =
        seq {
            for row = 0 to dim-1 do
                for col = 0 to dim-1 do
                    yield row, col
        }
        |> Seq.skip 1 // skip (0, 0)
    
    let changes =
        cells
        |> Seq.sumBy (fun (row, col) ->
            let current = sumMatrix.[row, col]
            let newMin  = calNewMin row col
            if newMin < current then
                sumMatrix.[row, col] <- newMin
                1
            else 0)

    if changes > 0 then optimize ()

optimize()

let answer = sumMatrix.[dim-1, dim-1]