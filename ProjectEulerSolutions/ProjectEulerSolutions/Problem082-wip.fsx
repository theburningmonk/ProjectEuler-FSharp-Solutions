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
        if col = 0 then matrix.[row, col] else 0)
    |> Array2D.init dim dim 

let fromTopDown row col = 
    sumMatrix.[row-1, col-1] + 
    matrix.[row-1, col] + 
    matrix.[row, col]
let fromBottomUp row col = 
    sumMatrix.[row+1, col-1] + 
    matrix.[row+1, col] +
    matrix.[row, col]
let fromLeft row col = 
    sumMatrix.[row, col-1] + 
    matrix.[row, col]

// for each position (row, col) find the shortest sum leading to 
// this point by moving down, up or right from the previous 
// point in the path
for col = 1 to dim-1 do
    for row = 0 to dim-1 do
        let left = fromLeft row col

        match (row, col) with
        | (0, _) -> 
            let bottomUp = fromBottomUp row col
            sumMatrix.[row, col] <- min left bottomUp
        | (_, _) when row = dim-1 -> 
            let topDown = fromTopDown row col
            sumMatrix.[row, col] <- min left topDown
        | (_, _) -> 
            let topDown  = fromTopDown row col
            let bottomUp = fromBottomUp row col
            let minSum   = min left <| min topDown bottomUp
            sumMatrix.[row, col] <- minSum
        
let answer = 
    [| 0..dim-1 |] 
    |> Array.map (fun row -> sumMatrix.[row, dim-1]) 
    |> Array.min