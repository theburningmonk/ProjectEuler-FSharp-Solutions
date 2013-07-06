open System.IO
open System.Collections.Generic
 
// read the keylog file into a int [] []
let entries = File.ReadAllLines(@"c:\temp\keylog.txt") 
              |> Array.map (fun str -> str.ToCharArray() |> Array.map (fun c -> int(c.ToString())))
 
// dictionary for keeping track of the list of numbers that falls behind a number
let inFrontOf = new Dictionary<int, int list>();
 
/// function to add the specified int list to the list of numbers that falls behind n
let addInFrontOf n (lst : int list) = 
    if inFrontOf.ContainsKey n then inFrontOf.[n] <- set(lst @ inFrontOf.[n]) |> Set.toList
    else inFrontOf.[n] <- set(lst) |> Set.toList
 
// analyse the login attempts and populate the dictionary
entries
|> Array.iter (fun arr ->
    addInFrontOf arr.[0] [arr.[1]; arr.[2]] 
    addInFrontOf arr.[1] [arr.[2]]
    addInFrontOf arr.[2] [])
 
let answer = inFrontOf 
             |> Seq.sortBy (fun kvp -> kvp.Value.Length) 
             |> Seq.map (fun kvp -> kvp.Key) 
             |> Seq.toList
             |> List.rev