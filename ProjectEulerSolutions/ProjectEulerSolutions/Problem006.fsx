let numbers = [|1..100|]
 
let sumOfSquares = numbers |> Array.map (fun x -> x * x) |> Array.sum
 
let sum = numbers |> Array.sum
let squareOfSum = sum * sum
 
let diff = squareOfSum - sumOfSquares