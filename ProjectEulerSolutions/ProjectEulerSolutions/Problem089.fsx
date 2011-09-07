open System.IO

// define the set of available roman numerals
let romanNumerals = [(['I'],1I);(['I';'V'],4I);(['V'],5I);(['I';'X'],9I);
                     (['X'],10I);(['X';'L'],40I);(['L'],50I);(['X';'C'],90I); 
                     (['C'],100I);(['C';'D'],400I);(['D'],500I);(['C';'M'],900I);
                     (['M'],1000I)]

// define function to get the numeric value of a roman numeral
let getNumericValue (numeral:char) = 
    snd (romanNumerals |> Seq.filter (fun (l, v) -> l.Length = 1 && l.[0] = numeral) |> Seq.head)

// define function to convert a roman numerals to its corresponding numeric value
let toNumeric (numerals:string) =
    let rec toNumericRec ((head::tail):char list) (num:bigint) =
        match head, tail with
        | _, [] -> num + getNumericValue head
        | _, hd::tl when getNumericValue head >= getNumericValue hd ->
            toNumericRec tail (num + getNumericValue head)
        | _, hd::[] -> // subtractive pair as last number i.e. XIX - 19
            num + getNumericValue hd - getNumericValue head
        | _, hd::tl -> // subtractive pair not as last number XCV - 95
            toNumericRec tl (num + getNumericValue hd - getNumericValue head)

    toNumericRec (numerals.ToCharArray() |> Array.toList) 0I

// converts an integer value to corresponding minimal roman numerals form
let toMinimalRomanNumerals (value:bigint) =
    let rec toMinimalRomanNumeralsRec (value:bigint) (list:char list) = 
        if value = 0I then list
        else 
            let (list', value') = 
                romanNumerals 
                |> List.sortBy (fun (l, v) -> -v) 
                |> Seq.filter (fun (l, v) -> v <= value) 
                |> Seq.head
            toMinimalRomanNumeralsRec (value-value') (list@list')

    toMinimalRomanNumeralsRec value []

let original = File.ReadAllLines(@"C:\TEMP\roman.txt")
let minimal = original |> Array.map toNumeric |> Array.map toMinimalRomanNumerals

let answer = 
    let originalLen = original |> Array.sumBy (fun str -> str.Length)
    let minimalLen = minimal |> Array.sumBy (fun str -> str.Length)
    originalLen - minimalLen