open System.IO

#time

// using the algorithm from SO: 
//  http://stackoverflow.com/a/2049593/55074

let sign (p1x, p1y) (p2x, p2y) (p3x, p3y) =
    (p1x - p3x) * (p2y - p3y) - (p2x - p3x) * (p1y - p3y)

let isIn p v1 v2 v3 =
    let b1 = sign p v1 v2 < 0.0
    let b2 = sign p v2 v3 < 0.0
    let b3 = sign p v3 v1 < 0.0

    b1 = b2 && b2 = b3

let answer = 
    __SOURCE_DIRECTORY__ + "\Problem102_input.txt"
    |> File.ReadAllLines 
    |> Array.map (fun s -> 
        s.Split ',' 
        |> Array.map float 
        |> Array.chunkBySize 2
        |> Array.map (fun [| x; y|] -> x, y))
    |> Array.filter (fun [| v0; v1; v2 |] ->
        isIn (0.0, 0.0) v0 v1 v2)
    |> Array.length