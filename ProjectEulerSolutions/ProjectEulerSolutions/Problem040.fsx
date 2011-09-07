// the base sequence of natural numbers
let naturalNumbers = Seq.unfold (fun x -> Some(x, x+1)) 1
 
// the decimal fraction 12345678910111213 as a sequence of strings
let fraction = 
    naturalNumbers 
    |> Seq.collect (fun x -> x.ToString().ToCharArray() |> Seq.map (fun c -> c.ToString()))
 
// define function d
let d n = int(fraction |> Seq.nth (n-1))

let answer = [0..6] |> Seq.map (fun n -> d (pown 10 n)) |> Seq.reduce (fun acc item -> acc * item)