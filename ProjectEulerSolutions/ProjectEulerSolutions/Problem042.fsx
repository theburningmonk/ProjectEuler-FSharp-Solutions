open System.IO
 
// read the content of the file into an array of words 
let words =
    File.ReadAllLines (@"C:\temp\words.txt")
    |> Array.map (fun s -> s.Replace("\"", ""))
    |> Array.collect (fun s -> s.ToUpper().Trim().Split(','))
 
// function to calculate the word value of a word
let wordValue word =
    word.ToString().ToCharArray()
    |> Array.map (fun c -> int(c) - int('A') + 1)
    |> Array.sum
 
// convert all words to their corresponding word values
let wordValues = words |> Array.map wordValue
let maxWordValue = wordValues |> Array.max
 
// the sequence of triangle numbers
let naturalNumbers = Seq.unfold (fun state -> Some(state, state + 1)) 1
let T n = n * (n + 1) / 2
let TSeq = naturalNumbers |> Seq.map T

let answer =
    TSeq
    |> Seq.takeWhile (fun t -> t <= maxWordValue)
    |> Seq.map (fun t -> wordValues |> Array.filter (fun n -> n = t) |> Array.length)
    |> Seq.sum