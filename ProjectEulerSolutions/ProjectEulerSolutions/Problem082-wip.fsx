open System
open System.IO

let dimension = 80

// load the original matrix
let matrix = array2D (File.ReadAllLines(@"c:\temp\matrix.txt") 
                      |> Array.map (fun l -> l.Split(',') |> Array.map int32))

// init the shortes sum matrix
let sumMatrix = Array2D.init dimension dimension (fun i j -> if j = 0 then matrix.[i, j] else 0)

let fromTop i j    = sumMatrix.[i - 1, j] + matrix.[i, j]
let fromBelow i j  = sumMatrix.[i + 1, j] + matrix.[i, j]
let fromLeft i j   = sumMatrix.[i, j - 1] + matrix.[i, j]

// for each position (i, j) find the shortest sum leading to this point by moving down or
// right from the previous point in the path
for i = 0 to dimension - 1 do
    for j = 0 to dimension - 1 do
        match (i, j) with
        | (_, 0) -> ()
        | (0, _) -> sumMatrix.[i, j] <- min (fromLeft i j) (fromBelow i j)
        | (_, _) when i < (dimension - 1)
                 -> sumMatrix.[i, j] <- min (fromLeft i j) (min (fromTop i j) (fromBelow i j))
        | (_, _) -> sumMatrix.[i, j] <- min (fromLeft i j) (fromTop i j)
        
let answer = [| 0..dimension-1 |] |> Array.map (fun i -> sumMatrix.[i, dimension-1]) |> Array.min