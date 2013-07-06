open System
open System.IO
 
// the top 10 most common words in English
let mostCommmonWords = ["the"; "be"; "to"; "of"; "and"; "a"; "in"; "that"; "have"; "I"]
 
// read the encrypted ASCII codes
let cipherBytes = File.ReadAllText(@"c:\temp\cipher1.txt").Split(',') 
                  |> Array.map (fun str -> byte(str.Trim()))
 
/// deciphers the byte array with the encryption key and returns the deciphered text
let decipher (bytes : byte[]) (keys : byte[]) = 
    new String(bytes |> Array.mapi (fun i uy -> uy ^^^ keys.[i % keys.Length]) |> Array.map char)
 
/// determines whether the specified text contains common English words
let predicate (str : string) = mostCommmonWords |> List.forall (fun str' -> str.Contains(str'))
 
// find the original text
let originalText =
    seq {
        for i in 0uy..255uy do
            for j in 0uy..255uy do
                for k in 0uy..255uy do
                    yield decipher cipherBytes [|i; j; k|]
    } 
    |> Seq.filter predicate
    |> Seq.head
 
let answer = originalText.ToCharArray() |> Array.map int |> Array.sum