open System
open System.IO

let dimension = 80

// load the original matrix
let matrix = array2D (File.ReadAllLines(@"c:\temp\matrix.txt") 
                      |> Array.map (fun l -> l.Split(',') |> Array.map int32))

// init the shortes sum matrix
let sumMatrix = 
    Array2D.init dimension dimension (fun i j -> if i = 0 && j = 0 then matrix.[i, j] else 0)

let fromTop i j = sumMatrix.[i - 1, j] + matrix.[i, j]
let fromLeft i j = sumMatrix.[i, j - 1] + matrix.[i, j]

// for each position (i, j) find the shortest sum leading to this point by moving down or
// right from the previous point in the path
for i = 0 to dimension - 1 do
    for j = 0 to dimension - 1 do
        match (i, j) with
        | (0, 0) -> ()
        | (_, 0) -> sumMatrix.[i, j] <- fromTop i j
        | (0, _) -> sumMatrix.[i, j] <- fromLeft i j
        | (_, _) -> sumMatrix.[i, j] <- Math.Min((fromTop i j), (fromLeft i j))

let answer = sumMatrix.[dimension - 1, dimension - 1]